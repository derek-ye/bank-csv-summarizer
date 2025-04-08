{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    let (month, day, year) = dateStringToIntArray txnDate
    pure $ fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)

-- converting datestring into an array like ["MM", "DD", "YYYY"], then to integer array
-- handles all validation and errors
dateStringToIntArray :: String -> (Int, Int, Int)
dateStringToIntArray dateStr = (monthNum, dayNum, yearNum)
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
