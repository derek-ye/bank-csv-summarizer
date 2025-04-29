{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Citi where
import Data.Csv
    ( (.:), FromNamedRecord(..) )
import Data.Text hiding (length)
import Data.Time (Day)
import TransactionCategorizer.Utils.Date (mmddyyyyDateParser)
import qualified TransactionCategorizer.BankParsers.Transaction as Trans
import Data.Maybe (fromMaybe)

data CitiTransaction = MkCitiTransaction {
  status :: Text,
  date :: Day,
  description :: Text,
  debit :: Maybe Double,      -- card payments
  credit :: Maybe Double      -- card transaction amounts
} deriving (Show)

instance FromNamedRecord CitiTransaction where
  parseNamedRecord r = MkCitiTransaction 
    <$> r .: "Status"
    <*> mmddyyyyDateParser "Date" r
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
  } = Trans.MkTransaction { Trans.transactionDate=citiDate, Trans.description=citiDescription, Trans.category=Nothing, Trans.amount=creditAmt }

  where
    -- ignore payments for now, count them as 0.0
    creditAmt = fromMaybe 0.0 citiCredit


