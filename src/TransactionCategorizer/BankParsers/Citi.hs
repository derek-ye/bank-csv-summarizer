{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Citi where
import Data.Csv
    ( (.:), FromNamedRecord(..) )
import Data.Text hiding (length)
import Data.Time (Day, fromGregorian)
import TransactionCategorizer.Utils.Date (mmddyyDateParser)

data CitiTransaction = MkCitiTransaction {
  status :: Text,
  date :: Day,
  description :: Text,
  debit :: Double,
  credit :: Double
} deriving (Show)

instance FromNamedRecord CitiTransaction where
  parseNamedRecord r = MkCitiTransaction 
    <$> r .: "Status"
    <*> mmddyyDateParser "Transaction Date" r
    <*> r .: "Description"
    <*> r .: "Debit"
    <*> r .: "Credit"
