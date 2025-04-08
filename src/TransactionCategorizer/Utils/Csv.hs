{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.Utils.Csv where

import qualified Data.ByteString.Lazy as BL
import Data.Csv as Csv
-- import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..))
import TransactionCategorizer.BankParsers.Transaction (Transaction)
import Data.Vector (Vector, empty)

-- readCSV :: FilePath -> IO ()
-- readCSV fileName = do
--     csvData <- BL.readFile fileName
--     case byteStringToCsv csvData of
--         Left err -> print err
--         Right csv -> print csv

byteStringToCsv :: BL.ByteString -> Either String (Vector Transaction)
byteStringToCsv csvBytestring = case Csv.decodeByName csvBytestring of
    Left err -> Left err
    Right (_, csv) -> Right csv

removeHeader :: Either String (Csv.Header, Vector a) -> Either String (Vector a)
removeHeader result = 
    case result of
        Left err -> Left err
        Right (_, records) -> Right records

addDummyHeader :: Either String (Vector a) -> Either String (Csv.Header, Vector a)
addDummyHeader result =
    case result of
    Left err -> Left err
    Right records -> Right (empty, records)
