{-# LANGUAGE CPP #-}
module Application where

import Data.List (intercalate)
import Data.String (fromString)
import Data.Maybe (fromMaybe)
import Network.Wai.Parse (parseRequestBody, getRequestBodyType, parseRequestBody, RequestBodyType(..), lbsBackEnd, fileContent)
import Network.Wai (Request(..), Response(..), Application)
import Network.HTTP.Types (ok200, seeOther303, badRequest400, Status, ResponseHeaders)
import Network.Wai.Util (string, stringHeaders, json, bodyBytestring, redirect', handleAcceptTypes)
import Web.PathPieces (PathPiece(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Base58Address (RippleAddress)
import Control.Error (readMay, headMay)
import Control.Monad.Trans (liftIO)
import Database.SQLite.Simple (Connection, query)
import Data.Binary (decodeOrFail)
import Network.URI (URI(..))
import Network.URI.Partial (relativeTo)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LZ
import qualified Data.ByteString as BS

import Records
import MustacheTemplates
import Sqlite3
#include "PathHelpers.hs"

-- Orphan instances, do not import this module!

instance PathPiece RippleAddress where
	fromPathPiece = readMay . T.unpack
	toPathPiece = T.pack . show

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

responseTextBuilder :: Status -> ResponseHeaders -> TL.Builder -> Response
responseTextBuilder s h = ResponseBuilder s h . Blaze.fromLazyText . TL.toLazyText

home :: URI -> Connection -> Application
home root _ _ =
	return $ responseTextBuilder ok200 headers (viewHome htmlEscape $ HomeRec [Form $ forPath `relativeTo` root])
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

for :: URI -> Connection -> Application
for root _ req = case adr of
		Just adr -> redirect' seeOther303 [] (reportForPath adr `relativeTo` root)
		Nothing -> string badRequest400 [] "Invalid Ripple.com address.\n"
	where
	adr = readMay =<< fmap T.unpack adrS
	adrS = fmap (T.decodeUtf8 . fromMaybe BS.empty) $ lookup (fromString "address") (queryString req)

reportFor :: URI -> Connection -> RippleAddress -> Application
reportFor _ db adr req = case gen of
	Just x -> do
		time <- liftIO (fmap floor getPOSIXTime :: IO Integer)
		string ok200 disp (show time ++ ": " ++ show adr ++ " " ++ T.unpack x)
	Nothing -> do
		assertions <- liftIO $ query db (fromString "SELECT `from`, `fromFingerprint`, `to`, `at`, `asserted`, `assertion` FROM assertions WHERE `to` = ?") [adr]
		handleAcceptTypes [
			("text/html",
				return $ responseTextBuilder ok200 headers (viewReport htmlEscape $ Report adr assertions)),
			("application/json",
				json ok200 [] (Report adr assertions))
			] req
	where
	gen = fmap (T.decodeUtf8 . fromMaybe BS.empty) $ lookup (fromString "newAssertion") (queryString req)
	Just disp = stringHeaders [("Content-Disposition", "attachment; filename=assertion.txt")]
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

assertFor :: URI -> Connection -> RippleAddress -> Application
assertFor root db adr req = do
	-- TODO: force adr to be the address of the object

	body <- case getRequestBodyType req of
		Just (Multipart _) -> fmap (maybe LZ.empty (fileContent . snd) . headMay . snd) (parseRequestBody lbsBackEnd req)
		_ -> fmap (LZ.fromChunks . (:[])) (bodyBytestring req)

	result <- liftIO $ case decodeOrFail body of
		Left _ -> return $ Left "Post data is not a valid OpenPGP message."
		Right (_,_,x) -> insertVerifiedAssertion db x

	case result of
		Left e -> handleAcceptTypes (errResp e) req
		Right () -> handleAcceptTypes resp req
	where
	errText e = string badRequest400 [] (e ++ "\n")

	errResp e = [
		("text/plain", errText e),
		("application/json",
			json ok200 [] (Aeson.object [
				(Aeson..=) (T.pack "error") (Aeson.toJSON e)
			]))
		]

	resp = [
		("text/html",
			redirect' seeOther303 [] (reportForPath adr `relativeTo` root)),
		("text/plain", string ok200 [] "success"),
		("application/json",
			json ok200 [] (Aeson.object [
				(Aeson..=) (T.pack "status") "success"
			]))
		]
