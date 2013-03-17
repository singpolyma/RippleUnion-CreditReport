module VerifyObject where

import Control.Monad (void, guard)
import Control.Error (readMay, hush)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Base58Address (RippleAddress)
import Control.Applicative ((*>))
import Data.Attoparsec.Text (Parser, parseOnly, decimal, string, takeTill, space, endOfLine, endOfInput)
import Data.Attoparsec.Combinator (choice)
import Data.Char (isSpace)
import Data.Time.Clock (UTCTime, diffUTCTime)
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
-- TODO: reject revoked keys
verifyObject :: UTCTime -> OpenPGP.Message -> OpenPGP.Message -> Maybe (OpenPGP.Packet, Object)
verifyObject time keys msg = listToMaybe $
	mapMaybe (objectFromVerifiedSig validKeys) verifiedSigs
	where
	verifiedSigs = map (OpenPGP.verify validKeys) (OpenPGP.signatures msg)
	validKeys = OpenPGP.Message $ map fst $
		filter (maybe True (\e -> e `diffUTCTime` time > 0) . snd)
			(keyExpirations keys)

-- Given a particular verified signature, extract the object
objectFromVerifiedSig :: OpenPGP.Message -> OpenPGP.SignatureOver -> Maybe (OpenPGP.Packet, Object)
objectFromVerifiedSig keys (OpenPGP.DataSignature (OpenPGP.LiteralDataPacket {
		OpenPGP.content = bytes
	}) [sig]) = do
		guard (signatureExpiry sig == Nothing) -- Reject expiring signatures
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

-- | unhashed expiry is the same as no expiry
-- If no creation time, also get a Nothing
-- Return value is since POSIX epoch
signatureExpiry :: OpenPGP.Packet -> Maybe Integer
signatureExpiry p | OpenPGP.isSignaturePacket p = do
	let pkts = OpenPGP.hashed_subpackets p
	creationTime <- listToMaybe (mapMaybe creationTimeSubpacket pkts)
	expiryAfter <- listToMaybe (mapMaybe expirySubpacket pkts)
	return $! (creationTime + expiryAfter)
signatureExpiry _ = Nothing

creationTimeSubpacket :: OpenPGP.SignatureSubpacket -> Maybe Integer
creationTimeSubpacket (OpenPGP.SignatureCreationTimePacket secs) =
	Just $ fromIntegral secs
creationTimeSubpacket _ = Nothing

expirySubpacket :: OpenPGP.SignatureSubpacket -> Maybe Integer
expirySubpacket (OpenPGP.SignatureExpirationTimePacket secs) =
	Just $ fromIntegral secs
expirySubpacket _ = Nothing

keyExpirySubpacket :: OpenPGP.SignatureSubpacket -> Maybe Integer
keyExpirySubpacket (OpenPGP.KeyExpirationTimePacket secs) =
	Just $ fromIntegral secs
keyExpirySubpacket _ = Nothing

keyExpirations :: OpenPGP.Message -> [(OpenPGP.Packet, Maybe UTCTime)]
keyExpirations = mapMaybe keyExpirationSignature . OpenPGP.signatures

-- | Assumes key packet
keyAndExpiryToTime :: (Integral a) => OpenPGP.Packet -> a -> UTCTime
keyAndExpiryToTime k expiry =
	posixSecondsToUTCTime $ (realToFrac $ OpenPGP.timestamp k) + (realToFrac expiry)

keyExpirationSignature :: OpenPGP.SignatureOver -> Maybe (OpenPGP.Packet, Maybe UTCTime)
keyExpirationSignature (OpenPGP.DataSignature {}) = Nothing
keyExpirationSignature s
	| null subpackets = Nothing -- So valid self-signature
	| otherwise = Just $ maybe (k, Nothing)
		((,)k . Just . keyAndExpiryToTime k) $
			listToMaybe $ mapMaybe keyExpirySubpacket subpackets
	where
	subpackets = concatMap OpenPGP.hashed_subpackets verifiedSelfSigs
	verifiedSelfSigs = OpenPGP.signatures_over $
		OpenPGP.verify (OpenPGP.Message [k]) s
	k = case s of
		OpenPGP.SubkeySignature {} -> OpenPGP.subkey s
		_ -> OpenPGP.topkey s

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
