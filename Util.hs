module Util where

import Control.Error (readMay, tryIO, eitherT)
import Data.Binary (Binary, decodeOrFail)
import qualified Data.ByteString.Lazy as LZ

tryIO' :: (IOError -> e) -> IO (Either e b) -> IO (Either e b)
tryIO' mapErr io = eitherT (return . Left . mapErr) return (tryIO io)

decodeM :: (Binary a, Monad m) => LZ.ByteString -> m a
decodeM bytes = case decodeOrFail bytes of
	Left (_,_,e) -> fail e
	Right (_,_,x) -> return x

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"
