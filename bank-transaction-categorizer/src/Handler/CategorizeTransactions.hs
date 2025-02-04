{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.CategorizeTransactions where

import Import
import GHC.Generics
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import qualified Data.ByteString.Lazy as LBS

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler Value
postCategorizeTransactionsR = do
    bytes <- rawRequestBody C.$$ CL.fold BS.append BS.empty
    let bodyStr = LBS8.toString $ LBS.fromStrict bytes
    _ <- Import.traceM bodyStr

    pure $ toJSON result 
    where
        result = CategorizeTransactionsResult { categorizedTransactions = Map.fromList [("key1", "Food"), ("key2", "Groceries")] }