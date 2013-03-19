module Auth where

import Numeric (showFFloat)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Word (Word32)
import Data.Base58Address (RippleAddress)
import Network.URI (URI(..), URIAuth(..), escapeURIString, isUnescapedInURIComponent)
import Crypto.Random (CryptoRandomGen, GenError)
import Control.Monad.CryptoRandom (crandomR)
import Data.Attoparsec.Combinator (option)
import Data.Attoparsec.Text (Parser, parseOnly, decimal, string, endOfInput)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP

data Destination = Destination {
		name           :: String,
		address        :: RippleAddress,
		destinationTag :: Word32
	} deriving (Eq, Show)

createAuthMessage :: (CryptoRandomGen g) => OpenPGP.Message -> Destination -> g -> Either GenError (OpenPGP.Message, g)
createAuthMessage k dest g = do
	(uri, g') <- randomTinyTransferURI dest g
	let msg = compressedDataMessage OpenPGP.BZip2 'u' "authURI.txt" 0
		(TL.encodeUtf8 $ TL.pack $ show (httpPrefixRippleURI uri) ++ "\n")
	OpenPGP.encrypt [] k OpenPGP.AES128 msg g'

compressedDataMessage :: OpenPGP.CompressionAlgorithm -> Char -> String -> Word32 -> LZ.ByteString -> OpenPGP.Message
compressedDataMessage compress format filename time content =
	OpenPGP.Message [OpenPGP.CompressedDataPacket compress (
			OpenPGP.Message [
				OpenPGP.LiteralDataPacket format filename time content
			]
		)]

escape :: String -> String
escape = escapeURIString isUnescapedInURIComponent

randomTinyAmount :: (CryptoRandomGen g) => g -> Either GenError (Int, g)
randomTinyAmount = crandomR (1,599999)

randomTinyTransferURI :: (CryptoRandomGen g) => Destination -> g -> Either GenError (URI, g)
randomTinyTransferURI (Destination name to dt) gen = do
	(drops, g') <- randomTinyAmount gen
	let xrp = (fromIntegral drops / 1000000) :: Float
	let q = "?to=" ++ show to ++ "&amnt=" ++ showFFloat Nothing xrp
		("/XRP&name=" ++ escape name ++ "&dt=" ++ show dt)
	return (URI "ripple:" Nothing "send" q "", g')

httpPrefixRippleURI :: URI -> URI
httpPrefixRippleURI u@(URI {uriPath = pth}) =
	u {
		uriScheme = "https:",
		uriAuthority = Just (URIAuth "" "ripple.com" ""),
		uriPath = maybe ("//" ++ pth) (const pth) (stripPrefix "//" pth)
	}

-- Always return drops
xrpAmountParser :: Parser Integer
xrpAmountParser = do
	int <- decimal
	frac <- option Nothing (fmap Just (string (T.pack ".") >> Attoparsec.takeWhile isDigit))
	void $ option T.empty (string $ T.pack "/XRP")
	endOfInput
	case frac of
		Nothing -> return $! int
		Just f
			| T.length f > 6 -> fail "Too many decimal places (only support up to drops)"
			| otherwise -> case parseOnly decimal f of
				Left _ -> fail "Decimal part not a number."
				Right d -> return $!
					(int * 1000000) + ((d * 1000000) `div` (10 ^ T.length f))
