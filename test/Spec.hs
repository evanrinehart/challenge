{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid ((<>))
import Data.Either
import System.IO
import System.Exit
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Char
import Data.Attoparsec.Text

import Common
import Types
import Encoder
import Parser
import Validator
import Editor
import Input

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

instance Arbitrary ValidOps where
  arbitrary = fmap VO generateOps

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

data BadEncoding = BadEncoding
  { beBytes :: ByteString
  , beBadWord8 :: Word8 }
    deriving Show

instance Arbitrary BadEncoding where
  arbitrary = do
    ops <- arbitrary :: Gen ValidOps
    let goodBytes = encodeUtf8 (encode ops)
    badWord8 <- arbitrary `suchThat` (not . validByte) :: Gen Word8
    i <- choose (0, BS.length goodBytes)
    let badBytes = BS.pack (insertAt i badWord8 (BS.unpack goodBytes)) 
    return (BadEncoding badBytes badWord8)

-- spot check the validByte function
w8 = fromIntegral . ord
w8ok = validByte . w8
w8bad = not . validByte . w8
prop_validByte1 = w8ok 'a'
prop_validByte2 = w8ok 'z'
prop_validByte3 = w8ok ' '
prop_validByte4 = w8ok 'j'
prop_validByte5 = w8ok '0'
prop_validByte6 = w8ok '9'
prop_validByte7 = w8ok '4'
prop_validByte8 = w8bad (pred 'a')
prop_validByte9 = w8bad (succ 'z')
prop_validByte10 = w8bad (pred ' ')
prop_validByte11 = w8bad (succ ' ')
prop_validByte12 = w8bad (pred '0')
prop_validByte13 = w8bad (succ '9')
prop_validByte14 = w8bad 'V'
prop_validByte15 = w8bad '#'
prop_validByte16 = not (validByte 137)
prop_validByte17 = not (validByte 255)
prop_validByte18 = not (validByte 0)

-- test the validateByteString function
prop_validateByteStringYes :: ValidOps -> Bool
prop_validateByteStringYes ops =
  validateByteString goodBytes == Right goodText where
    goodBytes = encodeUtf8 (encode ops)
    goodText = decodeUtf8 goodBytes

prop_validateByteStringNo (BadEncoding bytes badWord8) =
  validateByteString bytes == Left badWord8


-- test parseInput
prop_decodeEncodeIsRight ops = (parseInput . encode) ops == Right ops

prop_goodFileDoesParse = parseInput goodFile == Right goodData where
  goodFile = 
    T.unlines [
      "4",
      "1 example",
      "2 5",
      "3 1",
      "4"
    ]
  Right goodData = validateOps [Append "example", Delete 5, Print 1, Undo]

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

prop_badFileNoParse7 = isLeft (parseInput "")

-- test the numberBetween parser

prop_numberBetween1 =
  parseOnly (numberBetween 3 5) "3" == Right 3

prop_numberBetween2 =
  parseOnly (numberBetween 3 5) "5" == Right 5

prop_numberBetween3 = isLeft (parseOnly (numberBetween 3 5) "2")
prop_numberBetween4 = isLeft (parseOnly (numberBetween 3 5) "6")


-- test validateSession
prop_validateSessionYes =
  isRight $ validateSession (VO [Undo]) ("a",[""]) 

prop_validateSessionNo1 =
  isLeft $ validateSession (VO [Undo]) ("",[])

prop_validateSessionNo2 =
  isLeft $ validateSession (VO [Delete 3]) ("ab",[""])

prop_validateSessionNo3 =
  isLeft $ validateSession (VO [Print 3]) ("ab",[""])
  

-- test the edit function
prop_appendAppends (LA str) (ES (s,ss)) =
  edit (Append str) (s,ss) == (Nothing, (s <> str, s:ss))

prop_deleteDeletes = edit (Delete 3) ("example", []) == (Nothing, ("exam", ["example"]))

prop_printPrints = edit (Print 4) ("example", []) == (Just 'm', ("example", []))

prop_undoUndoes = edit Undo ("exam", ["example"]) == (Nothing, ("example", []))

-- test the runEditor function
prop_goodFileEdits = runEditor session == ("e", ("example",[""])) where
  Right session = validateSession ops ("",[])
  Right ops = parseInput goodFile
  goodFile = T.unlines [
      "4",
      "1 example",
      "2 5",
      "3 1",
      "4"
    ]

-- weird TH quickcheck hack
return []
runTests = $quickCheckAll

main :: IO ()
main = do
  ok <- runTests
  unless ok $ do
    hPutStrLn stderr "test suite failed"
    exitFailure


