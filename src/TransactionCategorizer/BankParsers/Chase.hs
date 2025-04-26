{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.BankParsers.Chase where
import Data.Time (Day)
import qualified Data.Text as T
import Data.Csv
    ( (.:), FromNamedRecord(..), )
import qualified TransactionCategorizer.BankParsers.Transaction as Trans
import TransactionCategorizer.Utils.Date (mmddyyyyDateParser)

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
        <$> mmddyyyyDateParser "Transaction Date" r
        <*> mmddyyyyDateParser "Post Date" r
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
