{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.QuickCheck
import qualified Data.Text as T
import Data.Monoid ((<>))

import Types
import Encoder
import Parser
import Editor

prop_RevRev xs = reverse (reverse xs) == xs

generateOps :: Gen [Op]
generateOps = begin where
  begin = do
    let s0 = ("", []) :: EditorState
    n <- choose (0, 100) :: Gen Int
    loop n s0
  loop 0 _ = return []
  loop n s = do
    op <- chooseOp s
    let (_,s') = edit op s
    ops <- loop (n - 1) s'
    return (op : ops)

chooseOp :: EditorState -> Gen Op
chooseOp (s,ss) = do
  t <- choose (1, 4) :: Gen Int
  case t of
    1 -> do
      n <- choose (1, 10)
      str <- fmap T.pack (vectorOf n (arbitrary `suchThat` isLowerAlpha))
      return (Append str)
    2 -> do
      if s == ""
        then chooseOp (s, ss)
        else fmap Delete (choose (1, T.length s))
    3 -> if s == ""
      then chooseOp (s,ss)
      else fmap Print (choose (1, T.length s))
    4 -> if ss == []
      then chooseOp (s, ss)
      else return Undo

newtype OpList = OpList { getOps :: [Op] }
  deriving Show

instance Arbitrary OpList where
  arbitrary = fmap OpList generateOps

newtype ES = ES { unES :: EditorState }
  deriving Show

instance Arbitrary ES where
  arbitrary = do
    n <- choose (1,100)
    str <- fmap T.pack (vectorOf n (arbitrary `suchThat` isLowerAlpha))
    return (ES (str, []))

newtype LowerAlpha = LA { unLA :: T.Text }
  deriving Show

instance Arbitrary LowerAlpha where
  arbitrary = do
    n <- choose (1, 10)
    str <- fmap T.pack (vectorOf n (arbitrary `suchThat` isLowerAlpha))
    return (LA str)

prop_decodeEncodeIsRight (OpList ops) = (parseInput . encode) ops == Right ops

prop_appendAppends (LA str) (ES (s,ss)) =
  edit (Append str) (s,ss) == (Nothing, (s <> str, s:ss))

prop_deleteDeletes = edit (Delete 3) ("example", []) == (Nothing, ("exam", ["example"]))

prop_printPrints = edit (Print 4) ("example", []) == (Just 'm', ("example", []))

prop_undoUndoes = edit Undo ("exam", ["example"]) == (Nothing, ("example", []))


