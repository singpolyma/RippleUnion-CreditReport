module Records where

import Data.Base58Address (RippleAddress)
import qualified Data.Text.Buildable as TL
import qualified Data.Text.Format.Types as TL

import Sqlite3

instance TL.Buildable RippleAddress where
	build adr = TL.build (TL.Shown adr)

data Report = Report {
		address :: RippleAddress,
		assertions :: [AssertionRow]
	}
