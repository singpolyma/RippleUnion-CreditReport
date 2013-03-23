module Main (main) where

import Prelude hiding (FilePath)
import Control.Monad (void)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..))
import Control.Error (err)
import Filesystem.Path (FilePath)
import Filesystem (getWorkingDirectory)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Database.SQLite.Simple (withConnection)

import Network.Wai.Dispatch
import Routes

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

main :: IO ()
main = main' . map (fmap addTrailingSlash . parseAbsoluteURI) =<< getArgs
	where
	main' [Just root@(URI {uriAuthority = Just _})] = do
		cwd <- getWorkingDirectory
		void $ withConnection "./dev.db"
			(\db -> run 3000 $
				logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
				dispatch (staticRoot cwd) $ routes root db)        -- Do routing
	main' _ = err "Usage: ./Main <Root URI>"
