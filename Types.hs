module Types where

import Data.Text (Text)

data Op =
  Append Text |
  Delete Int |
  Print Int |
  Undo
    deriving (Eq, Ord, Show, Read)

