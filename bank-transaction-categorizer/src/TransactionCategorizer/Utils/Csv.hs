{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.Utils.Csv where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Csv
    ( FromNamedRecord, decodeByName )
import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..))
import TransactionCategorizer.BankParsers.Transaction (Transaction)
import Data.Vector

-- readCSV :: FilePath -> IO ()
-- readCSV fileName = do
--     csvData <- BL.readFile fileName
--     case byteStringToCsv csvData of
--         Left err -> print err
--         Right csv -> print csv

byteStringToCsv :: BL.ByteString -> Either String (Vector Transaction)
byteStringToCsv csvBytestring = case decodeByName csvBytestring of
    Left err -> Left err
    Right (_, csv) -> Right csv