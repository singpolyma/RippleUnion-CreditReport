module Websocket where

import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Base58Address (RippleAddress)
import Control.Error (readMay)
import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Data.Text as T

wsSendJSON :: (WS.TextProtocol p, Aeson.ToJSON j) => j -> WS.WebSockets p ()
wsSendJSON = WS.sendTextData . Aeson.encode

wsReceiveJSON :: (WS.TextProtocol p, Aeson.FromJSON j) => WS.WebSockets p (Maybe j)
wsReceiveJSON = fmap (Aeson.decode) WS.receiveData

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"

data AccountLine = AccountLine {
		lineAccount   :: RippleAddress,
		lineBalance   :: Double,
		lineCurrency  :: String,
		lineLimit     :: Double,
		lineLimitPeer :: Double
	}
	deriving (Eq, Show)

instance Aeson.FromJSON AccountLine where
	parseJSON (Aeson.Object o) = AccountLine          <$>
			(readM =<< (Aeson..:) o (T.pack "account")) <*>
			(readM =<< (Aeson..:) o (T.pack "balance")) <*>
			(Aeson..:) o (T.pack "currency")            <*>
			(readM =<< (Aeson..:) o (T.pack "limit"))   <*>
			(readM =<< (Aeson..:) o (T.pack "limit_peer"))
	parseJSON _ = fail "AccountLine is always a JSON object"

data AccountLinesR = AccountLinesR RippleAddress [AccountLine]
	deriving (Eq, Show)

instance Aeson.FromJSON AccountLinesR where
	parseJSON (Aeson.Object o) = do
		result <- (Aeson..:) o (T.pack "result")
		account <- readM =<< (Aeson..:) result (T.pack "account")
		lines <- (Aeson..:) result (T.pack "lines")
		linesDecoded <- mapM Aeson.parseJSON lines
		return $ AccountLinesR account linesDecoded
	parseJSON _ = fail "AccountLinesR is always a JSON object"

data Commands =
	AccountLinesC RippleAddress

instance Aeson.ToJSON Commands where
	toJSON (AccountLinesC adr) = Aeson.object [
			(Aeson..=) (T.pack "command") (T.pack "account_lines"),
			(Aeson..=) (T.pack "account") (T.pack $ show adr)
		]

app :: WS.WebSockets WS.Hybi10 ()
app = do
	-- Ask for this account's direct trust lines
	wsSendJSON (AccountLinesC $ read "r3ADD8kXSUKHd6zTCKfnKT3zV9EZHjzp1S")
	v <- wsReceiveJSON -- Get the response
	liftIO $ print (v :: Maybe AccountLinesR) -- Print the response

-- Connect to ripple.com and run the test app
doit = WS.connect "s1.ripple.com" 51233 "/" app
