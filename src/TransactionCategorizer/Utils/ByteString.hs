module TransactionCategorizer.Utils.ByteString where

import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

stringToByteString :: String -> BS.ByteString
stringToByteString = TE.encodeUtf8 . T.pack