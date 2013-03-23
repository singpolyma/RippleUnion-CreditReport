module Records where

import Control.Applicative ((<$>), (<*>))
import Control.Error (readMay, hush, tryHead, noteT, EitherT(..), MaybeT(..), hoistMaybe, throwT)
import Data.Base58Address (RippleAddress)
import qualified Data.Text.Buildable as TL
import qualified Data.Text.Format.Types as TL
import Database.SQLite.Simple (query, field, FromRow(..), ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Data.Time.Clock (UTCTime)
import Data.Binary (Binary, decodeOrFail, encode)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Lazy as LZ
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Base64.Lazy as B64

import Assertion

instance TL.Buildable RippleAddress where
	build = TL.build . TL.Shown

instance TL.Buildable AssertionType where
	build = TL.build . TL.Shown

instance ToField RippleAddress where
	toField adr = toField (show adr)

data Report = Report {
		address :: RippleAddress,
		assertions :: [FormattedAssertionRow]
	}
	deriving (Show, Eq)

instance Aeson.ToJSON Report where
	toJSON (Report adr asserts) = Aeson.object [
			(Aeson..=) (T.pack "for") (T.pack $ show adr),
			(Aeson..=) (T.pack "assertions") asserts
		]

data FormattedAssertionRow = FormattedAssertionRow {
		at8601 :: String,
		atHuman :: String,
		keyId :: String,
		signedAssertion :: String,
		row :: [AssertionRow]
	}
	deriving (Show, Eq)

instance Aeson.ToJSON FormattedAssertionRow where
	toJSON = Aeson.toJSON . head . row

formatAssertionRow :: AssertionRow -> FormattedAssertionRow
formatAssertionRow row =
	FormattedAssertionRow iso8601 human keyId signed [row]
	where
	signed = map (toEnum.fromEnum) $ LZ.unpack $ B64.encode $ encode $ assertion row
	keyId = reverse $ take 8 $ reverse $ fromFingerprint row
	iso8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (at row)
	human = formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (at row)

data AssertionRow = AssertionRow {
		from :: OpenPGP.Message,
		fromFingerprint :: String,
		to :: RippleAddress,
		at :: UTCTime,
		asserted :: AssertionType,
		assertion :: OpenPGP.Message
	}
	deriving (Show, Eq)

instance Aeson.ToJSON AssertionRow where
	toJSON (AssertionRow from _ _ at asserted assertion) = Aeson.object [
			(Aeson..=) (T.pack "from") (B64.encode $ encode from),
			(Aeson..=) (T.pack "at") at,
			(Aeson..=) (T.pack "asserted") (show asserted),
			(Aeson..=) (T.pack "signedAssertion") (B64.encode $ encode assertion)
		]

instance FromRow FormattedAssertionRow where
	fromRow = fmap formatAssertionRow fromRow

instance FromRow AssertionRow where
	fromRow = AssertionRow <$> (field >>= decodeM) <*> field <*>
			(field >>= readM) <*> field <*> (field >>= readM) <*>
			(field >>= decodeM)

instance ToRow AssertionRow where
	toRow row = map ($row) [
			toField . encode . from,
			toField . fromFingerprint,
			toField . to,
			toField . at,
			toField . show . asserted,
			toField . encode . assertion
		]

decodeM :: (Binary a, Monad m) => LZ.ByteString -> m a
decodeM bytes = case decodeOrFail bytes of
	Left (_,_,e) -> fail e
	Right (_,_,x) -> return x

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"
