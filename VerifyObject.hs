module VerifyObject where

import Control.Monad (void)
import Control.Error (readMay, hush)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Base58Address (RippleAddress)
import Control.Applicative ((*>))
import Data.Attoparsec.Text (Parser, parseOnly, decimal, string, takeTill, space, endOfLine, endOfInput)
import Data.Attoparsec.Combinator (choice)
import Data.Char (isSpace)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LZ
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP

-- | Assertions users can make about each other
data Object =
	MadePayment UTCTime RippleAddress |
	MissedPayment UTCTime RippleAddress |
	NotTrusted UTCTime RippleAddress
	deriving (Eq)

objectAddress :: Object -> RippleAddress
objectAddress (MadePayment _ adr) = adr
objectAddress (MissedPayment _ adr) = adr
objectAddress (NotTrusted _ adr) = adr

objectTime :: Object -> UTCTime
objectTime (MadePayment t _) = t
objectTime (MissedPayment t _) = t
objectTime (NotTrusted t _) = t

-- | Do OpenPGP verification and extract an object from a message
-- TODO: reject expired/revoked keys
-- TODO: REJECT EXPIRING SIGS!  SIGS LIVE FOREVER!
verifyObject :: OpenPGP.Message -> OpenPGP.Message -> Maybe (OpenPGP.Packet, Object)
verifyObject keys msg = listToMaybe $
	mapMaybe (objectFromVerifiedSig keys) verifiedSigs
	where
	verifiedSigs = map (OpenPGP.verify keys) (OpenPGP.signatures msg)

-- Given a particular verified signature, extract the object
objectFromVerifiedSig :: OpenPGP.Message -> OpenPGP.SignatureOver -> Maybe (OpenPGP.Packet, Object)
objectFromVerifiedSig keys (OpenPGP.DataSignature (OpenPGP.LiteralDataPacket {
		OpenPGP.content = bytes
	}) [sig]) = do
		key <- issuerKey keys sig
		text <- hush $ T.decodeUtf8' $ BS.concat $ LZ.toChunks $ bytes
		object <- hush (parseOnly objectParser text)
		return (key, object)
objectFromVerifiedSig _ _ = Nothing

-- | Helper to get the key that made a particular signature
issuerKey :: OpenPGP.Message -> OpenPGP.Packet -> Maybe OpenPGP.Packet
issuerKey keys sig = do
	issuer <- OpenPGP.signature_issuer sig
	OpenPGP.find_key OpenPGP.fingerprint keys issuer

-- Parse our objects from text
objectParser :: Parser Object
objectParser = do
	time <- fmap (posixSecondsToUTCTime . realToFrac) decimal
	void $ string (T.pack ": ")
	adr <- fmap T.unpack $ takeTill isSpace
	decodedAdr <- case readMay adr of
		Just x -> return x
		Nothing -> fail $ adr ++ " is not a valid Ripple address."
	void space
	cons <- choice [
			string (T.pack "made a payment") *> return MadePayment,
			string (T.pack "missed a payment") *> return MissedPayment,
			string (T.pack "not trusted") *> return NotTrusted
		]
	endOfLine
	endOfInput
	return $ cons time decodedAdr
