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

import VerifyObject
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

tryFind :: (Monad m) => e -> (a -> Bool) -> [a] -> EitherT e m a
tryFind e p xs = noteT e $ hoistMaybe $ find p xs

processObject :: Connection -> OpenPGP.Message -> IO (Either String (RippleAddress, Object))
processObject conn msg = runEitherT $ do
	time <- liftIO $ getCurrentTime
	r <- liftIO $ findByKeyId conn (issuerKeyIds msg)
	(adr,obj) <- tryHead "No valid signed object found." $
		mapMaybe (\(AddressAndKey adr k) ->
			fmap (first (const adr)) (verifyObject time k msg)
		) r

	when (objectTime obj > time) (throwT "Signed object claims to be from the future.")
	when (time `diffUTCTime` objectTime obj > 3600) (throwT "Signed object is too old.")

	AccountLinesR _ lines <- noteT (show adr ++ " has no credit relationships.") $ MaybeT $ doit adr
	-- TODO: might have more than one credit relationship
	line <- tryFind (show adr ++ " has no credit relationship with " ++ show (objectAddress obj))
		((== objectAddress obj) . lineAccount) lines

	-- TODO: refuse objects from A to B too close together

	case obj of
		MadePayment _ _-> return (adr, obj)
		MissedPayment _ _ | lineBalance line > 0 -> return (adr, obj)
		NotTrusted _ _ | lineBalance line > 0 -> return (adr, obj)
		_ -> throwT (show (objectAddress obj) ++ " is not in debt to " ++ show adr)
