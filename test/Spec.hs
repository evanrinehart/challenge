{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Either
import System.IO
import System.Exit

import Types
import Encoder
import Parser
import Editor

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


prop_goodFileDoesParse = parseInput goodFile == Right goodData where
  goodFile = 
    T.unlines [
      "4",
      "1 example",
      "2 5",
      "3 1",
      "4"
    ]
  goodData = [Append "example", Delete 5, Print 1, Undo]

prop_badFileNoParse1 = isLeft (parseInput badFile1) where
  badFile1 = T.unlines [
      "1",
      "2 1000001" -- delete too many
    ]


prop_badFileNoParse2 = isLeft (parseInput badFile2) where
  badFile2 = T.unlines [
      "2", -- wrong number of lines
      "2 1000000" 
    ]

prop_badFileNoParse3 = isLeft (parseInput badFile3) where
  badFile3 = T.unlines [
      "2", -- wrong number of lines
      "1 foo",
      "2 2",
      "1 ruit"
    ]

prop_badFileNoParse4 = isLeft (parseInput badFile4) where
  badFile4 = T.unlines [
      "1",
      "0 wrong" -- op 0 is invalid
    ]

prop_badFileNoParse5 = isLeft (parseInput badFile5) where
  badFile5 = T.unlines [
      "2",
      "1 fruit",
      "2 0" -- no delete 0
    ]

prop_badFileNoParse6 = isLeft (parseInput badFile6) where
  badFile6 = T.unlines [
      "2",
      "1 fruit",
      "3 0" -- no print 0
    ]

prop_goodFileEdits = runEditor ops ("",[]) == ("e", ("example",[""])) where
  Right ops = parseInput goodFile
  goodFile = T.unlines [
      "4",
      "1 example",
      "2 5",
      "3 1",
      "4"
    ]

return []
runTests = $quickCheckAll

main :: IO ()
main = do
  ok <- runTests
  if ok
    then return ()
    else do
      hPutStrLn stderr "test suite failed"
      exitFailure

