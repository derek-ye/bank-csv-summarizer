{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Citi where
import Data.Csv
    ( (.:), FromNamedRecord(..) )
import Data.Text hiding (length)
import Data.Time (Day)
import TransactionCategorizer.Utils.Date (mmddyyDateParser)
import qualified TransactionCategorizer.BankParsers.Transaction as Trans

data CitiTransaction = MkCitiTransaction {
  status :: Text,
  date :: Day,
  description :: Text,
  debit :: Double,      -- card payments
  credit :: Double      -- card transaction amounts
} deriving (Show)

instance FromNamedRecord CitiTransaction where
  parseNamedRecord r = MkCitiTransaction 
    <$> r .: "Status"
    <*> mmddyyDateParser "Transaction Date" r
    <*> r .: "Description"
    <*> r .: "Debit"
    <*> r .: "Credit"

toTransaction :: CitiTransaction -> Trans.Transaction
toTransaction MkCitiTransaction
  { status = _
  , date = citiDate
  , description = citiDescription
  , debit = _
  , credit = citiCredit
  } = Trans.MkTransaction { Trans.transactionDate=citiDate, Trans.description=citiDescription, Trans.category=Nothing, Trans.amount=citiCredit }


