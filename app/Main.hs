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
import Text.Read (readMaybe)
import qualified Data.Text.Read as TR

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
        <$> parseChaseDateField "Transaction Date" r
        <*> parseChaseDateField "Post Date" r
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

-- Chase dates are in the format YYYY/MM/DD
parseChaseDateField :: T.Text -> NamedRecord -> Parser Day
parseChaseDateField fieldName r = do
        let fieldNameBS = TE.encodeUtf8 fieldName
        -- why can't i do `txnDate :: T.Text <- ...`?
        txnDate <- r .: fieldNameBS :: Parser T.Text
        [year, month, day] <- parseInt <$> T.splitOn "/" txnDate      -- ["YYYY", "MM", "DD"]
        pure $ fromGregorian year month day
    where
        parseInt :: T.Text -> Int
        parseInt dateTxt =
            case TR.decimal dateTxt of
                Right (n, _) -> n -- success case, use n :: Int
                Left err -> fail $ "Unknown date format: " ++ err -- handle error case

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

