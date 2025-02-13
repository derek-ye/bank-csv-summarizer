{-# LANGUAGE OverloadedStrings #-}
module BankParsers.Chase where

import Data.Time (Day, fromGregorian)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Csv
    ( (.:), FromNamedRecord(..), Parser, NamedRecord )
import qualified Data.Text.Read as TR

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

instance BankTransaction ChaseTransaction where
    getAmount = amount
    printTransaction = show

-- Chase dates are in the format YYYY/MM/DD
parseChaseDateField :: T.Text -> NamedRecord -> Parser Day
parseChaseDateField fieldName r = do
        let fieldNameBS = TE.encodeUtf8 fieldName
        txnDate <- r .: fieldNameBS     -- txnDate is a date string that should look like '07/16/2023'
        let (month, day, year) = dateStringToIntArray txnDate
        pure $ fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)

-- converting datestring into an array like ["MM", "DD", "YYYY"], then to integer array
-- handles all validation and errors
dateStringToIntArray :: String -> (Int, Int, Int)
dateStringToIntArray dateStr = (yearNum, monthNum, dayNum)
    where
        textArr :: [T.Text]
        textArr = T.splitOn "/" (T.pack dateStr)

        intArr :: [Either String (Int, T.Text)]
        intArr = TR.decimal <$> textArr

        throwDateCouldNotBeParsedError = error "Date string could not be parsed"
        (month, day, year) = case intArr of
            [m, d, y] -> (m, d, y)
            _ -> throwDateCouldNotBeParsedError
        monthNum = case month of
            Left _ -> throwDateCouldNotBeParsedError
            Right (mn, _) -> mn
        dayNum = case day of
            Left _ -> throwDateCouldNotBeParsedError
            Right (dn, _) -> dn
        yearNum = case year of
            Left _ -> throwDateCouldNotBeParsedError
            Right (yn, _) -> yn

data ChaseCardTransactionType = Credit | Debit deriving (Show)
