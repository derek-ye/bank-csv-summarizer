{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.BankParsers.Transaction where

import TransactionCategorizer.BankParsers.Chase (ChaseTransaction(..), ChaseCardTransactionType(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv (NamedRecord, Parser, FromNamedRecord(..), decodeByName)
import Data.Foldable (toList)
import TransactionCategorizer.Utils.FakeData
import Data.Text.Encoding (decodeUtf8)
import qualified Data.HashMap.Strict as HM
import Debug.Trace

data Transaction = Chase ChaseTransaction deriving Show

instance FromNamedRecord Transaction where
    parseNamedRecord r = case detectBankType r of
        ChaseBank -> Chase <$> parseNamedRecord r

data BankType = ChaseBank | UnknownBank deriving (Show, Eq)

-- A named record looks something like this:
--
-- [ ("Name", "John Doe")
-- , ("Age", "30")
-- , ("Email", "john@example.com")
-- ]

detectBankType :: NamedRecord -> BankType
detectBankType record =
    -- get the keys from the hashmap as bytestring, then map each one to a text
    let headers = map decodeUtf8 $ HM.keys record
    in trace (show headers) $ case () of
        _ | all (`elem` headers) chaseHeaders -> ChaseBank
          | otherwise -> UnknownBank
  where
    chaseHeaders = ["Transaction Date", "Post Date", "Description", "Category", "Amount", "Memo"]