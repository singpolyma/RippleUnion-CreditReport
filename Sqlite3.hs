module Sqlite3 where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Control.Error (readMay, hush)
import Data.Base58Address (RippleAddress)
import Database.SQLite.Simple (query, field, FromRow(..), Connection)
import Data.Binary (Binary, decodeOrFail)
import Control.Exception (try)
import qualified Data.OpenPGP as OpenPGP
import qualified Data.ByteString.Lazy as LZ

data KeyAndAddress = KeyAndAddress RippleAddress OpenPGP.Message
	deriving (Show, Eq)

instance FromRow KeyAndAddress where
	fromRow = KeyAndAddress <$> (field >>= readM) <*> (field >>= decodeM)

decodeM :: (Binary a, Monad m) => LZ.ByteString -> m a
decodeM bytes = case decodeOrFail bytes of
	Left (_,_,e) -> fail e
	Right (_,_,x) -> return x

-- | Signal read errors in some Monad (for parsing)
readM :: (Read r, Monad m) => String -> m r
readM s = case readMay s of
	Just x -> return x
	Nothing -> fail $ s ++ " is invalid"

findByKeyId :: Connection -> String -> IO (Maybe KeyAndAddress)
findByKeyId conn keyId = fmap listToMaybe $ query conn q ['%' : keyId]
	where
	q = fromString "SELECT ripple_address, keydata FROM keys WHERE fingerprint LIKE ?"
