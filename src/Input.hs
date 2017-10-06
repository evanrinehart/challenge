module Input where

import Data.Text hiding (all)
import Data.Text.Encoding
import Control.Exception
import Data.Char
import qualified Data.ByteString as BS
import Control.Monad
import System.IO
import Data.Word
import Data.Typeable

loadInput :: Handle -> IO Text
loadInput h = do
  bytes <- BS.hGetContents h
  unless (all validByte (BS.unpack bytes)) $ do
    throwIO BadEncodingException
  return (decodeUtf8 bytes)

validByte :: Word8 -> Bool
validByte x = 
  let c = chr (fromIntegral x) in
  isLowerAlpha c ||
  isDigit c ||
  c == ' ' ||
  c == '\n' ||
  c == '\r'

isLowerAlpha :: Char -> Bool
isLowerAlpha c = 'a' <= c && c <= 'z'

data BadEncodingException = BadEncodingException 
  deriving (Typeable)

instance Show BadEncodingException where
  show _ = "BadEncodingException (unrecognized bytes in input)"

instance Exception BadEncodingException
