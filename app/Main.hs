{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (stdin)
import Control.Monad (forM_, unless)
import Control.Exception (throwIO)

import Input
import Parser
import Editor

main :: IO ()
main = do
  input <- loadInput stdin
  case parseInput input of
    Left errmsg -> throwIO (ParseFailedException errmsg)
    Right ops -> do
      let blankState = ("",[])
      unless (areOpsOk ops blankState) $
        throwIO BadOperationException
      let (outs, _) = runEditor ops blankState
      forM_ outs $ \c -> do
        putStrLn [c]
