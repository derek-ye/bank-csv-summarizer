{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.BankParsers.Transaction where

import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..), ChaseCardTransactionType(..))
import TransactionCategorizer.BankParsers.WellsFargo
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (NamedRecord, Parser, FromNamedRecord(..), decodeByName, (.:))
import Data.Foldable (toList)
import Data.Time (Day)
import Data.Text
import TransactionCategorizer.Utils.ByteString (charToWord8)
import Data.String
import TransactionCategorizer.BankParsers.Other.Day()

-- Our internal representation of a transaction. We convert from
-- different bank transaction csv formats to this type.
data Transaction = MkTransaction {
  transactionDate :: Day
  , description :: Text
  , category :: Maybe Text
  , amount :: Double
} deriving Show

instance FromNamedRecord Transaction where
  parseNamedRecord r = MkTransaction
    <$> r .: "transactionDate"
    <*> r .: "description"
    <*> r .: "category"
    <*> r .: "amount"

data BankType = ChaseBank | WellsFargoBank | UnknownBank deriving (Show, Eq)

detectBankType :: BL.ByteString -> BankType
detectBankType csvBS
  | isChaseHeader headers = ChaseBank
  | isWfHeader headers = WellsFargoBank
  | otherwise = UnknownBank
  where
    headers = BL.takeWhile (== charToWord8 '\n') csvBS

-- Helper detection functions
isChaseHeader :: (Eq a, Data.String.IsString a) => a -> Bool
isChaseHeader headers = headers == "Transaction Date,Post Date,Description,Category,Amount,Memo"

isWfHeader :: BL.ByteString -> Bool
isWfHeader headers = BL.count (charToWord8 ',') headers == 4
