module Auth where

import Numeric (showFFloat)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Word (Word32)
import Data.Base58Address (RippleAddress)
import Network.URI (URI(..), URIAuth(..), escapeURIString, isUnescapedInURIComponent)
import Crypto.Random (CryptoRandomGen, GenError)
import Control.Monad.CryptoRandom (crandomR)
import Data.Attoparsec.Combinator (option)
import Data.Attoparsec.Text (Parser, parseOnly, decimal, string, endOfInput)
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as T

data Destination = Destination {
		name           :: String,
		address        :: RippleAddress,
		destinationTag :: Word32
	} deriving (Eq, Show)

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
		uriPath = "//" ++ pth
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
