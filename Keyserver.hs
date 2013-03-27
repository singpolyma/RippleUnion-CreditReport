module Keyserver where

import Control.Error (hush, MaybeT(..), runMaybeT, hoistMaybe, headMay)
import Data.Binary (decodeOrFail)
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Stream as HTTP (ConnError(..))
import qualified Data.OpenPGP as OpenPGP
import qualified Codec.Encryption.OpenPGP.ASCIIArmor as ASCIIArmor
import qualified Codec.Encryption.OpenPGP.ASCIIArmor.Types as ASCIIArmor
import qualified Data.ByteString.Lazy as LZ

import Util

fetchKey :: String -> IO (Maybe OpenPGP.Message)
fetchKey fpr = runMaybeT $ do
	r <- MaybeT $ fmap hush $ tryIO' (HTTP.ErrorMisc . show) $ HTTP.simpleHTTP req
	-- XXX: Data is all ASCII, but this is still a terrible hack
	let rbytes = LZ.pack $ map (toEnum.fromEnum) (HTTP.rspBody r)
	armor <- hoistMaybe $ headMay =<< hush (ASCIIArmor.decodeLazy rbytes :: Either String [ASCIIArmor.Armor])
	bytes <- case armor of
		ASCIIArmor.Armor ASCIIArmor.ArmorPublicKeyBlock _ bytes -> return bytes
		_ -> hoistMaybe Nothing
	case decodeOrFail bytes of
		Left _ -> hoistMaybe Nothing
		Right (_,_,x) -> return x
	where
	req = HTTP.getRequest $ "http://singpolyma.net:11371/pks/lookup?op=get&search=0x" ++ fpr ++ "&exact=on&options=mr"
