{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.BankParsers.Transaction where

import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..), ChaseCardTransactionType(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (Header, Parser, FromNamedRecord, decodeByName)
import Data.Foldable (toList)
import Debug.Trace
import TransactionCategorizer.Utils.FakeData
import Data.Text.Encoding (decodeUtf8)

class BankTransaction a where
    -- getDate :: a -> Day
    getAmount :: a -> Double
    printTransaction :: a -> String

data Transaction = forall a. BankTransaction a => Transaction a
instance Show Transaction where
    show (Transaction t) = printTransaction t

data BankType = Chase

detectBankFromHeaders :: Header -> Either String BankType
detectBankFromHeaders headers = 
    let headerNames = map decodeUtf8 (V.toList headers)
    in case () of
        _ | all (`elem` headerNames) chaseHeaders -> Right Chase
          | otherwise -> Left "Unknown bank format"
  where
    chaseHeaders = ["Transaction Date", "Post Date", "Description", "Category", "Amount", "Memo"]