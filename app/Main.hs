{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (stdin)
import Control.Monad (forM_)
import Control.Exception (throwIO)

import Types
import Input
import Parser
import Validator
import Editor

blankState :: EditorState
blankState = ("",[])

main :: IO ()
main = do
  input <- loadInput stdin
  case parseInput input of
    Left errmsg -> throwIO (ParseFailedException errmsg)
    Right ops -> case validateSession ops blankState of
      Left problem -> throwIO (BadOperationException problem)
      Right session -> do
        let (outs, _) = runEditor session
        forM_ outs $ \c -> do
          putStrLn [c]
