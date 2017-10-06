{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hPutStrLn, stderr)
import Control.Monad (forM_)
import qualified Data.Text.IO as T

import Types
import Parser
import Editor

main :: IO ()
main = do
  input <- T.getContents
  case parseInput input of
    Left errmsg -> do
      hPutStrLn stderr errmsg
    Right ops -> do
      let (outs, _) = runEditor ops ("",[])
      forM_ outs $ \c -> do
        putStrLn [c]
