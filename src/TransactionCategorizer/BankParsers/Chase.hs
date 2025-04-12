{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Chase where

import Data.Time (Day, fromGregorian)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Csv
    ( (.:), FromNamedRecord(..), Parser, NamedRecord )
import qualified Data.Text.Read as TR
import qualified TransactionCategorizer.BankParsers.Transaction as Trans

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

-- Chase dates are in the format MM/DD/YYYY
parseChaseDateField :: T.Text -> NamedRecord -> Parser Day
parseChaseDateField fieldName r = do
    let fieldNameBS = TE.encodeUtf8 fieldName
    txnDate <- r .: fieldNameBS     -- txnDate is a date string that should look like '07/16/2023'
    let (month, day, year) = case dateStringToIntArray txnDate of
                                Just date -> date
                                Nothing -> error "Unable to parse Chase date field"
    pure $ fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)

-- converting datestring into an array like ["MM", "DD", "YYYY"], then to integer array
-- handles all validation and errors
dateStringToIntArray :: String -> Maybe (Int, Int, Int)
dateStringToIntArray dateStr = do
    let textArr = T.splitOn "/" (T.pack dateStr)
    if length textArr == 3
        then case TR.decimal <$> textArr of
                    [Right (m, _), Right (d, _), Right (y, _)] -> Just (m, d, y)
                    _ -> Nothing
        else Nothing

toTransaction :: ChaseTransaction -> Trans.Transaction
toTransaction MkChaseTransaction { transactionDate = chaseTransactionDate
                                  , postDate = _
                                  , description = chaseDescription
                                  , category = chaseCategory
                                  , transactionType = _
                                  , amount=chaseAmount
                                  , memo = _
                                  } = Trans.MkTransaction { Trans.transactionDate=chaseTransactionDate, Trans.description=chaseDescription, Trans.category=Just chaseCategory, Trans.amount=chaseAmount }

data ChaseCardTransactionType = Credit | Debit deriving (Show)
