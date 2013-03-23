module Main (main) where

import Control.Monad (void)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..))
import Control.Error (err)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)

import Database.SQLite.Simple (withConnection)

import Network.Wai.Dispatch
import Routes
import Application

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

main :: IO ()
main = main' . map (fmap addTrailingSlash . parseAbsoluteURI) =<< getArgs
	where
	main' [Just root@(URI {uriAuthority = Just _})] =
		void $ withConnection "./dev.db"
			(\db -> run 3000 $
				logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
				dispatch on404 $ routes root db)                   -- Do routing
	main' _ = err "Usage: ./Main <Root URI>"
