module Input where

import Data.Text hiding (all)
import Data.Text.Encoding
import Control.Exception
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad
import Data.Word
import Data.Typeable
import qualified Data.List as L

import Common

-- | Attempt to decode a bytes string. If it detects an invalid byte that
-- byte is returned instead.
validateByteString :: ByteString -> Either Word8 Text
validateByteString bytes = case L.find (not . validByte) (BS.unpack bytes) of
  Nothing -> Right (decodeUtf8 bytes)
  Just w8 -> Left w8

-- | True if byte is allowed in the input format. False otherwise.
validByte :: Word8 -> Bool
validByte x = 
  let c = chr (fromIntegral x) in
  isLowerAlpha c ||
  isDigit c ||
  c == ' ' ||
  c == '\n' ||
  c == '\r'

data BadEncodingException = BadEncodingException
  { badByte      :: Word8
  , badByteChar8 :: Char }
    deriving (Show, Typeable)

instance Exception BadEncodingException
