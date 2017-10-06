{-# LANGUAGE OverloadedStrings #-}
module Encoder where

import qualified Data.Text as T
import Data.Text (Text, pack)
import Data.Monoid ((<>))

import Types

encode :: [Op] -> Text
encode ops =
  let l = length ops in
  let line1 = pack (show l) in
  T.unlines (pack (show l) : map encodeOp ops)

encodeOp (Append str) = "1 " <> str
encodeOp (Delete k)   = "2 " <> pack (show k)
encodeOp (Print k)    = "3 " <> pack (show k)
encodeOp Undo         = "4"
