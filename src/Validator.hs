{-# LANGUAGE BangPatterns #-}
module Validator where

import qualified Data.Text as T
import Control.Exception
import Data.Typeable

import Types

data BadOperation = BO
  { boLineNo :: Int
  , boMsg    :: String }
    deriving Show

data ValidationState = VS
  { currentSize :: Int
  , sizeHistory :: [Int] } deriving Show

-- | Check that a list of operations is valid against the given starting
-- editor state.
validateSession :: ValidOps -> EditorState -> Either BadOperation ValidEditorSession
validateSession vo@(VO ops) st@(current,history) = go 2 ops startingValidationState where
  startingValidationState = VS (T.length current) (map T.length history)
  go _       []       _  = Right (VES vo st)
  go !lineNo (op:ops) vs = case validateStep lineNo op vs of
    Right nextState -> go (lineNo+1) ops nextState
    Left problem    -> Left problem

validateStep :: Int -> Op -> ValidationState -> Either BadOperation ValidationState
validateStep lineNo op vs@(VS current history) = case op of
  Append x -> Right (VS (current + T.length x) (current:history))
  Delete n
    | n >= 1 && n <= current -> Right (VS (current - n) (current:history))
    | otherwise -> Left (BO lineNo ("delete operation out of bounds " ++ show n ++ " out of " ++ show current))
  Print n
    | n >= 1 && n <= current -> Right vs
    | otherwise -> Left (BO lineNo ("print operation out of bounds " ++ show n ++ " out of " ++ show current))
  Undo -> case history of
    []     -> Left (BO lineNo "undo with no history remaining")
    (x:xs) -> Right (VS x xs)

data BadOperationException = BadOperationException BadOperation
  deriving (Show, Typeable)

instance Exception BadOperationException
