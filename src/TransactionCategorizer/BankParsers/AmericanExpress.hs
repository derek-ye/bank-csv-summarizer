{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.AmericanExpress where

import Data.Time (Day)
import qualified Data.Text as T
import Data.Csv
    ( (.:), FromNamedRecord(..), )
import TransactionCategorizer.Utils.Date (mmddyyyyDateParser)
import qualified TransactionCategorizer.BankParsers.Transaction as Trans
import Data.Vector

data AmericanExpressTransaction = MkAmericanExpressTransaction {
    transactionDate :: Day,
    description :: T.Text,
    amount :: Double
} deriving (Show)

instance FromNamedRecord AmericanExpressTransaction where
    parseNamedRecord r = MkAmericanExpressTransaction 
        <$> mmddyyyyDateParser "Date" r
        <*> r .: "Description"
        <*> r .: "Amount"

toTransaction :: AmericanExpressTransaction -> Trans.Transaction
toTransaction MkAmericanExpressTransaction { transactionDate = amexTransactionDate
                                  , description = amexDescription
                                  , amount = amexAmount
                                  } = Trans.MkTransaction { Trans.transactionDate=amexTransactionDate
                                                          , Trans.description=amexDescription
                                                          , Trans.amount=amexAmount
                                                          , Trans.category=Nothing
                                                          }

amexHandler :: Either String (Vector AmericanExpressTransaction) -> Vector Trans.Transaction
amexHandler (Left e) = error $ "Failed to parse Capital One csv: " <> e
amexHandler (Right transactions) = toTransaction <$> transactions
