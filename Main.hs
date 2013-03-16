module Main where

import Data.List (find)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Binary (decode)
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP

newtype RippleAddress = RippleAddress String
	deriving (Show, Eq)

userIDPacket :: RippleAddress -> OpenPGP.Packet
userIDPacket (RippleAddress addr) = OpenPGP.UserIDPacket addr

-- | Fails if signature does not match
-- returns address and key that signed it
extractRippleAddress :: OpenPGP.Message -> OpenPGP.Message -> Maybe (RippleAddress, OpenPGP.Packet)
extractRippleAddress keys msg = listToMaybe $
	mapMaybe (\(k,s) -> (flip (,) k) `fmap` addressFromSig s) $
		mapMaybe (addressSigs keys) (OpenPGP.signatures msg)

-- | Does not check validity of signature
addressFromSig :: OpenPGP.SignatureOver -> Maybe RippleAddress
addressFromSig (OpenPGP.DataSignature (OpenPGP.LiteralDataPacket {
		OpenPGP.content = msg
	}) _) =
	listToMaybe $ mapMaybe rippleUserID pkts
	where
	OpenPGP.Message pkts = decode msg
addressFromSig (OpenPGP.CertificationSignature _ pkt _) = rippleUserID pkt
addressFromSig _ = Nothing

rippleUserID :: OpenPGP.Packet -> Maybe RippleAddress
rippleUserID (OpenPGP.UserIDPacket adr@('r':_)) = Just (RippleAddress adr)
rippleUserID _ = Nothing

addressSigs :: OpenPGP.Message -> OpenPGP.SignatureOver -> Maybe (OpenPGP.Packet, OpenPGP.SignatureOver)
addressSigs keys sigs@(OpenPGP.DataSignature _ _) = case OpenPGP.verify keys sigs of
	s@(OpenPGP.DataSignature _ [sig]) -> do
		key <- issuerKey keys sig
		return $! (key, s)
	_ -> Nothing
addressSigs _ sigs@(OpenPGP.CertificationSignature k _ _) = -- We only care about self-sigs
	case OpenPGP.verify (OpenPGP.Message [k]) sigs of
		s@(OpenPGP.CertificationSignature _ _ (_:_)) -> Just (k, s)
		_ -> Nothing
addressSigs _ _ = Nothing

issuerKey :: OpenPGP.Message -> OpenPGP.Packet -> Maybe OpenPGP.Packet
issuerKey keys sig = do
	issuer <- OpenPGP.signature_issuer sig
	OpenPGP.find_key OpenPGP.fingerprint keys issuer
