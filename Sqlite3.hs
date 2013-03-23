module Sqlite3 where

import Control.Applicative
import Data.String (fromString)
import Control.Monad (when)
import Data.List (find)
import Control.Arrow (first)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime, diffUTCTime, addUTCTime)
import Control.Error (readMay, hush, tryHead, noteT, EitherT(..), MaybeT(..), hoistMaybe, throwT, headMay)
import Data.Base58Address (RippleAddress)
import Database.SQLite.Simple (query, field, FromRow(..), Connection, execute)
import Data.Binary (Binary, decodeOrFail)
import Control.Exception (try)
import Control.Monad.Trans (liftIO)
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP
import qualified Data.ByteString.Lazy as LZ

import Keyserver
import Records
import Assertion

issuerKeyIds :: OpenPGP.Message -> [String]
issuerKeyIds (OpenPGP.Message ((OpenPGP.CompressedDataPacket _ (OpenPGP.Message p1)):p2)) =
	issuerKeyIds (OpenPGP.Message (p1 ++ p2))
issuerKeyIds (OpenPGP.Message pkts) = mapMaybe OpenPGP.signature_issuer pkts

extractVerifiedAssertion :: OpenPGP.Message -> IO (Either String (OpenPGP.Packet, OpenPGP.Message, Assertion))
extractVerifiedAssertion msg = runEitherT $ do
	time <- liftIO $ getCurrentTime
	k <- noteT "Keyserver fetch failed." $ (MaybeT . fetchKey) =<< (hoistMaybe $ headMay (issuerKeyIds msg))
	(adr, obj@(assertion, target, at)) <- noteT "No valid signed object found." $ hoistMaybe $
		verifyAssertion time k msg

	when (at > time) (throwT "Signed object claims to be from the future.")
	when (time `diffUTCTime` at > 3600) (throwT "Signed object is too old.")

	return (adr, k, obj)

insertVerifiedAssertion :: Connection -> OpenPGP.Message -> IO (Either String ())
insertVerifiedAssertion conn msg = runEitherT $ do
	(key, keyM, (typ,to,time)) <- EitherT $ extractVerifiedAssertion msg
	let fpr = OpenPGP.fingerprint key
	let row = AssertionRow keyM fpr to time typ msg

	-- refuse objects from this key that are too close together
	r <- liftIO $ query conn (fromString "SELECT count(1) FROM assertions WHERE `at` > ? AND `at` < ?") (addUTCTime (-10) time, addUTCTime 10 time)
	case r of
		[[x]] | (x::Int) > 0 -> throwT "You have submitted other assertions too close in time to that assertion."
		_ -> return ()

	liftIO $ execute conn (fromString "INSERT INTO assertions VALUES (?,?,?,?,?,?)") row
