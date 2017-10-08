module Main where

import System.IO
import Control.Monad (forM_)
import Control.Exception (throwIO)
import Data.Text
import qualified Data.ByteString as BS
import Data.Char

import Types
import Input
import Parser
import Validator
import Editor

main :: IO ()
main = do
  txt <- load stdin
  ops <- parse txt
  veSession <- validate ops blankEditorState
  let (outs, _) = runEditor veSession
  forM_ outs $ \c -> do
    putStrLn [c]

load :: Handle -> IO Text
load h = do
  bytes <- BS.hGetContents h
  case validateByteString bytes of
    Left w8   -> throwIO (BadEncodingException w8 (chr (fromIntegral w8)))
    Right txt -> return txt

parse :: Text -> IO ValidOps
parse txt = case parseInput txt of
  Left errorMsg -> throwIO (ParseFailedException errorMsg)
  Right ops -> return ops 

validate :: ValidOps -> EditorState -> IO ValidEditorSession
validate ops st = case validateSession ops st of
  Left problem -> throwIO (BadOperationException problem)
  Right session -> return session

