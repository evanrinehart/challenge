{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (takeWhile)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Monad
import Control.Applicative
import Data.Maybe

import Control.Exception
import Data.Typeable

import Types

parseInput :: Text -> Either String [Op]
parseInput = parseOnly inputFile

inputFile :: Parser [Op]
inputFile = do
  spaces
  opCount <- decimal
  spaces
  endOfLine
  spaces
  ops <- count opCount operation
  endOfInput
  unless (appendsAreValid ops) (fail "sum of appended words too big")
  unless (deletesAreValid ops) (fail "sum of deletions too big")
  return ops

operation :: Parser Op
operation = do
  t <- numberBetween 1 4
  spaces
  case t of
    1 -> append  <?> "Append"
    2 -> delete  <?> "Delete"
    3 -> printOp <?> "Print"
    4 -> undo    <?> "Undo"
    _ -> error ("bug in parser, number not between 1 and 4 (" ++ show t ++ ")")

append :: Parser Op
append = do
  str <- takeWhile isLowerAlpha
  endOfOp
  spaces
  return (Append str)

delete :: Parser Op
delete = do
  k <- numberBetween 1 (10^6)
  when (k == 0) (fail "delete 0 is not allowed")
  spaces
  endOfOp
  spaces
  return (Delete k)

printOp :: Parser Op
printOp = do
  i <- numberBetween 1 (10^6)
  when (i == 0) (fail "print 0 is not allowed, 1 is the first character")
  spaces
  endOfOp
  spaces
  return (Print i)

undo :: Parser Op
undo = do
  spaces
  endOfOp
  spaces
  return Undo

numberBetween :: Int -> Int -> Parser Int
numberBetween a b = do
  i <- decimal
  when (i < a || i > b) $ do
    fail ("expected number in range [" ++ show a ++ "," ++ show b ++ "] (got " ++ show i ++ ")")
  return i

endOfOp :: Parser ()
endOfOp = do
  endOfLine <|> endOfInput
  return ()

spaces :: Parser ()
spaces = skipWhile isHorizontalSpace


isLowerAlpha :: Char -> Bool
isLowerAlpha c = c >= 'a' && c <= 'z'


appendsAreValid :: [Op] -> Bool
appendsAreValid ops =
  let words = catMaybes (map appendPayload ops) in
  sum (map T.length words) <= 10^6

deletesAreValid :: [Op] -> Bool
deletesAreValid ops =
  let deletes = catMaybes (map deleteCount ops) in
  sum deletes <= 2 * 10^6




data ParseFailedException = ParseFailedException String
  deriving (Typeable)

instance Show ParseFailedException where
  show (ParseFailedException msg) = "ParseFailedException (" ++ msg ++ ")"

instance Exception ParseFailedException
