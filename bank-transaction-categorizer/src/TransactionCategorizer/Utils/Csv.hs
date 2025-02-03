{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.Utils.Csv where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Csv
    ( decodeByName )
import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..))

readCSV :: FilePath -> IO ()
readCSV fileName = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error $ "Error encountered: " <> err
        Right (_, v) -> V.forM_ v $ \ txn ->
            putStrLn (T.unpack (description txn) <> " - " <> showGregorian (transactionDate txn))

-- FilePath -> ByteString
