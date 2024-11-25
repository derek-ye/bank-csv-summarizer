{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Time (Day, fromGregorian)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Csv
import Debug.Trace

data Transaction = ChaseTransaction

data ChaseTransaction = MkChaseTransaction {
    transactionDate :: Day,
    postDate :: Day,
    description :: T.Text,
    category :: T.Text,
    transactionType :: ChaseCardTransactionType,
    amount :: Double,
    memo :: T.Text
} deriving (Show)

instance FromNamedRecord ChaseTransaction where
    parseNamedRecord r = MkChaseTransaction 
        <$> parseDateField "Transaction Date" r
        <*> parseDateField "Post Date" r
        <*> r .: "Description"
        <*> r .: "Category"
        <*> parseNamedRecord r  -- type
        <*> r .: "Amount"
        <*> r .: "Memo"

instance FromNamedRecord ChaseCardTransactionType where
    parseNamedRecord r = do
        txnType <- r .: "Type"
        case txnType of
            "Credit" -> pure Credit
            "Debit" -> pure Debit
            _ -> fail $ "Unknown type: " ++ txnType

parseDateField :: T.Text -> NamedRecord -> Parser Day
parseDateField fieldName r = do
    let fieldNameBS = TE.encodeUtf8 fieldName
    txnDate <- r .: fieldNameBS :: Parser T.Text
    pure $ fromGregorian 2024 3 15

data ChaseCardTransactionType = Credit | Debit deriving (Show)

main :: IO ()
main = do
    result <- readCSV "chase-example.csv"
    pure ()

readCSV :: FilePath -> IO ()
readCSV fileName = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> putStrLn "wtf"
        Right (_, v) -> V.forM_ v $ \ txn ->
            putStrLn (T.unpack $ description txn <> " - " <> T.show (transactionDate txn))

