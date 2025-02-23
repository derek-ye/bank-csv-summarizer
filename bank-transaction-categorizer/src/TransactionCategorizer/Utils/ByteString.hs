module TransactionCategorizer.Utils.ByteString where

import Data.Word (Word8)
import Data.Char (ord)

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord