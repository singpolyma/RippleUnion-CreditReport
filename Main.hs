module Main (main) where

import Prelude hiding (FilePath)
import Control.Monad (void)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..))
import Control.Error (err, headMay)
import Filesystem.Path (FilePath)
import Filesystem (getWorkingDirectory)
import OpenSSL (withOpenSSL)

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
main = withOpenSSL $ do
	args <- getArgs
	let root = fmap addTrailingSlash (parseAbsoluteURI =<< headMay args)
	main' root args
	where
	main' (Just root@(URI {uriAuthority = Just _})) (_:dbpth:port:_) = do
		cwd <- getWorkingDirectory
		void $ withConnection dbpth
			(run (read port) .
				logStdoutDev . autohead . acceptOverride . jsonp . -- Middleware
				dispatch (staticRoot cwd) . routes root)           -- Do routing
	main' root@(Just (URI {uriAuthority = Just _})) (_:dbpth:_) =
		main' root [dbpth, "3000"]
	main' _ _ = err "Usage: ./Main <Root URI> <DB path> <port>"
