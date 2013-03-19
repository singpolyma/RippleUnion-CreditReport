module Sqlite3 where

import Control.Applicative
import Control.Monad (when)
import Data.List (find)
import Control.Arrow (first)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Error (readMay, hush, tryHead, noteT, EitherT(..), MaybeT(..), hoistMaybe, throwT)
import Data.Base58Address (RippleAddress)
import Database.SQLite.Simple (query, field, FromRow(..), Connection, open, close)
import Data.Binary (Binary, decodeOrFail)
import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import qualified Data.OpenPGP as OpenPGP
import qualified Data.ByteString.Lazy as LZ

import Assertion
import Websocket hiding (readM)

data AddressAndKey = AddressAndKey RippleAddress OpenPGP.Message
	deriving (Show, Eq)

instance FromRow AddressAndKey where
	fromRow = AddressAndKey <$> (field >>= readM) <*> (field >>= decodeM)

decodeM :: (Binary a, Monad m) => LZ.ByteString -> m a
decodeM bytes = case decodeOrFail bytes of
	Left (_,_,e) -> fail e
	Right (_,_,x) -> return x

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"

findByKeyId :: Connection -> [String] -> IO [AddressAndKey]
findByKeyId conn keyIds = query conn q (map ('%':) keyIds)
	where
	q = fromString $ "SELECT ripple_address, keydata FROM keys WHERE 1=0" ++
		concat (replicate (length keyIds) like)
	like = " OR fingerprint LIKE ?"

issuerKeyIds :: OpenPGP.Message -> [String]
issuerKeyIds (OpenPGP.Message ((OpenPGP.CompressedDataPacket _ (OpenPGP.Message p1)):p2)) =
	issuerKeyIds (OpenPGP.Message (p1 ++ p2))
issuerKeyIds (OpenPGP.Message pkts) = mapMaybe OpenPGP.signature_issuer pkts

processObject :: Connection -> OpenPGP.Message -> IO (Either String (RippleAddress, Assertion))
processObject conn msg = runEitherT $ do
	time <- liftIO $ getCurrentTime
	r <- liftIO $ findByKeyId conn (issuerKeyIds msg)
	(adr, obj@(assertion, target, at)) <- tryHead "No valid signed object found." $
		mapMaybe (\(AddressAndKey adr k) ->
			fmap (first (const adr)) (verifyAssertion time k msg)
		) r

	when (at > time) (throwT "Signed object claims to be from the future.")
	when (time `diffUTCTime` at > 3600) (throwT "Signed object is too old.")

	AccountLinesR _ lines <- noteT (show adr ++ " has no credit relationships.") $ MaybeT $ doit adr
	let line = filter ((== target) . lineAccount) lines
	when (null line) (throwT $ show adr ++ " has no credit relationship with " ++ show target)
	let isOwed = any ((>0). lineBalance) line

	-- TODO: refuse objects from A to B too close together

	case assertion of
		MadePayment -> return (adr, obj)
		Chargeback -> return (adr, obj)
		MissedPayment | isOwed -> return (adr, obj)
		NotTrusted | isOwed -> return (adr, obj)
		_ -> throwT (show target ++ " is not in debt to " ++ show adr)
