{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text (Text)
import Data.Attoparsec.Text
import Control.Monad
import Control.Applicative

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
  str <- takeTill (\c -> c == '\n' || c == '\r')
  endOfOp
  spaces
  return (Append str)

delete :: Parser Op
delete = do
  k <- decimal
  when (k == 0) (fail "delete 0 is not allowed")
  spaces
  endOfOp
  spaces
  return (Delete k)

printOp :: Parser Op
printOp = do
  i <- decimal
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
