{-# LANGUAGE OverloadedStrings #-}
module BankParsers.Chase where

import Data.Time (Day, fromGregorian)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Csv
    ( (.:), FromNamedRecord(..), Parser, NamedRecord )
import qualified Data.Text.Read as TR
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
        txnDate <- r .: fieldNameBS     -- date string should look like '07/16/2023'
        -- traceM $ "Debug message here" <> txnDate
        let (month:day:year:_) = TR.decimal <$> T.splitOn "/" (T.pack txnDate) :: [Either String (Int, T.Text)]      -- converting string into an array like ["MM", "DD", "YYYY"], then to integer array
        monthNum <- case month of
            Left _ -> pure 0
            Right (monthNum, _) -> pure monthNum
        dayNum <- case day of
            Left _ -> pure 0
            Right (dayNum, _) -> pure dayNum
        yearNum <- case year of
            Left _ -> pure 0
            Right (yearNum, _) -> pure yearNum
        pure $ fromGregorian (fromIntegral yearNum) (fromIntegral monthNum) (fromIntegral dayNum)

data ChaseCardTransactionType = Credit | Debit deriving (Show)
