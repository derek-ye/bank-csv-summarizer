{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.CategorizeTransactions where

import Import hiding ((.))
import GHC.Generics
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import TransactionCategorizer.BankParsers.Transaction (BankType(..), detectBankType)
import qualified Data.ByteString.Char8 as BS8
import TransactionCategorizer.BankParsers.Transaction (isChaseHeader)
import qualified Data.Csv as Csv
import TransactionCategorizer.BankParsers.Transaction
import qualified TransactionCategorizer.BankParsers.Chase as Chase
import qualified TransactionCategorizer.BankParsers.WellsFargo as WellsFargo
import TransactionCategorizer.Core.Categorizer (categorizeTransactions)
import qualified Data.Map as Map

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler Value
postCategorizeTransactionsR = do
    csvBS <- rawRequestBody C.$$ CL.fold BS.append BS.empty
    let bankType = detectBankType csvBS
    let abc = case bankType of
            ChaseBank -> Csv.decodeByName $ LBS.fromStrict csvBS
            WellsFargoBank -> Csv.decode Csv.NoHeader $ LBS.fromStrict csvBS
            UnknownBank -> notFound
    _ <- Import.traceM "Test"
    _ <- Import.traceM $ show $ BS8.takeWhile (/= '\n') csvBS
    _ <- Import.traceM $ show $ isChaseHeader $ show $ BS8.takeWhile (/= '\n') csvBS
    _ <- Import.traceM $ show $ detectBankType csvBS

    returnJson result 
    where
        result = CategorizeTransactionsResult { categorizedTransactions = Map.fromList [("key1", "Food"), ("key2", "Groceries")] }

categorizeChaseTransactions :: Vector Chase.ChaseTransaction -> Vector Transaction
categorizeChaseTransactions transactions = do
    -- must parse this into a maybe
    categoryMapStr <- categorizeTransactions $ toList (Chase.description <$> transactions)
    _ <- Import.traceM $ show categoryMapStr
    pure $ MkTransaction (fromGregorian 2025 10 10) "Test" Nothing 0.0

    where
        a = 5
        -- -- then figure out how to use this function
        -- assignCategories categoryMap Chase.ChaseTransaction{..} = ...
        -- -- then convert to a Transaction
        -- Transaction {category=fromMaybe "" (Map.lookup description categoryMap), ..}

-- handleWfCsv :: Vector WellsFargo.WellsFargoTransaction -> Vector Transaction
-- handleWfCsv csv = categorizeTransactions . WellsFargo.description <$> csv