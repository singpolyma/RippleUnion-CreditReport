module Application where

import Data.String (fromString)
import Network.Wai (Request(..), Response(..), Application)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, notAcceptable406, Status, ResponseHeaders)
import Network.Wai.Util (string, stringHeaders)
import Web.PathPieces (PathPiece(..))
import Data.Base58Address (RippleAddress)
import Control.Error (readMay)
import Control.Monad.Trans (liftIO)
import Database.SQLite.Simple (query, field, FromRow(..), Connection, open, close)
import Database.SQLite.Simple.ToField (ToField(..))
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL

import Records
import MustacheTemplates

-- Orphan instances, do not import this module!

instance PathPiece RippleAddress where
	fromPathPiece = readMay . T.unpack
	toPathPiece = T.pack . show

instance ToField RippleAddress where
	toField adr = toField (show adr)

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

on404 :: Application
on404 _ = string notFound404 [] "Not Found"

reportFor :: Connection -> RippleAddress -> Application
reportFor db adr req = do
		assertions <- liftIO $ query db (fromString "SELECT `to`, `from`, `type`, `time`, `signed` FROM assertions WHERE `to` = ?") [adr]
		return $ responseTextBuilder ok200 headers (viewReport htmlEscape $ Report adr assertions)
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]
