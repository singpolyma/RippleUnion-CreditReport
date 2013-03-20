module Main (main) where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)

import Network.Wai.Dispatch
import Routes
import Application

main :: IO ()
main = do
	putStrLn "Running..."
	run 3000 $
		logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
		dispatch on404 $ routes                            -- Do routing
