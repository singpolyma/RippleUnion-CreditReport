module OTC where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Stream as HTTP (ConnError(..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LZ

import Util

data OTCKey = OTCKey String String deriving (Eq, Show)

instance Aeson.FromJSON OTCKey where
	parseJSON (Aeson.Object o) = OTCKey                           <$>
			(fromMaybe "" <$> (Aeson..:?) o (T.pack "fingerprint")) <*>
			(fromMaybe "" <$> (Aeson..:?) o (T.pack "nick"))
	parseJSON _ = fail "OTCKey is always a JSON object"

otcKeyToTuple :: OTCKey -> Maybe (String, String)
otcKeyToTuple (OTCKey "" _) = Nothing
otcKeyToTuple (OTCKey fpr nick) = Just (fpr, nick)

otcKeys :: IO [(String, String)]
otcKeys = do
	r <- tryIO' (HTTP.ErrorMisc . show) $ HTTP.simpleHTTP req
	-- XXX: Data is all ASCII, but this is still a terrible hack
	let rbytes = LZ.pack . map (toEnum.fromEnum) . HTTP.rspBody <$> r
	case rbytes of
		Left _ -> return []
		Right x ->
			return $ mapMaybe otcKeyToTuple $ fromMaybe [] (Aeson.decode x)
	where
	req = HTTP.getRequest "http://bitcoin-otc.com/viewgpg.php?outformat=json"
