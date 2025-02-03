{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.CategorizeTransactions where

import Import
import GHC.Generics
import Data.Aeson (object)
import qualified Data.Map as Map

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler Value
postCategorizeTransactionsR = do
    pure $ toJSON result 
    where
        result = CategorizeTransactionsResult { categorizedTransactions = Map.fromList [("key1", "Food"), ("key2", "Groceries")] }