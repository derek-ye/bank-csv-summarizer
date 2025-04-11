{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module TransactionCategorizer.BankParsers.Transaction where

import qualified Data.ByteString as BS
import Data.Csv (FromNamedRecord(..), (.:))
import Data.Time (Day)
import Data.Text
import TransactionCategorizer.Utils.ByteString (charToWord8)
import Data.String
import TransactionCategorizer.BankParsers.Other.Day()
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Aeson as Aeson
import GHC.Generics


-- Our internal representation of a transaction. We convert from
-- different bank transaction csv formats to this type.
data Transaction = MkTransaction {
  transactionDate :: Day
  , description :: Text
  , category :: Maybe Text
  , amount :: Double
} deriving (Show, Generic)

instance FromNamedRecord Transaction where
  parseNamedRecord r = MkTransaction
    <$> r .: "transactionDate"
    <*> r .: "description"
    <*> r .: "category"
    <*> r .: "amount"

-- Typeclass to allow bank -> golden transaction conversions
class ToTransaction a where
  toTransaction :: a -> Transaction

-- Add new bank types here
data BankType = ChaseBank | WellsFargoBank | UnknownBank deriving (Show, Eq)

detectBankType :: BS.ByteString -> BankType
detectBankType csvBS
  | isChaseHeader headers = ChaseBank
  | isWfHeader headers = WellsFargoBank
  | otherwise = UnknownBank
  where
    headers = BS8.takeWhile (/= '\n') csvBS

-- Helper detection functions
isChaseHeader :: (Eq a, Data.String.IsString a) => a -> Bool
isChaseHeader headers = headers == "Transaction Date,Post Date,Description,Category,Type,Amount,Memo"

isWfHeader :: BS.ByteString -> Bool
isWfHeader headers = BS.count (charToWord8 ',') headers == 4

updateTransactionCategory :: Transaction -> Text -> Transaction
updateTransactionCategory MkTransaction { transactionDate=transactionDate
                                        , description=description
                                        , category=_oldCategory
                                        , amount=amount
                                        } newCategory = MkTransaction { transactionDate
                                                                      , description
                                                                      , category = Just newCategory
                                                                      , amount
                                                                      }

instance Aeson.ToJSON Transaction

