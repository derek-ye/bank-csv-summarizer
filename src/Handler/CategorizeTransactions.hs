{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.CategorizeTransactions where

import Import hiding ((.), zip)
import GHC.Generics
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import qualified Data.ByteString.Lazy as LBS
import TransactionCategorizer.BankParsers.Transaction (Transaction(..), BankType(..), detectBankType, isChaseHeader)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Csv as Csv
import TransactionCategorizer.BankParsers.Transaction
import qualified TransactionCategorizer.BankParsers.Chase as Chase
import qualified TransactionCategorizer.BankParsers.WellsFargo as WellsFargo
import TransactionCategorizer.Core.Categorizer (categorizeTransactions)
import qualified Data.Map as Map
import qualified Data.Vector as V
import TransactionCategorizer.Utils.Csv

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler Value
postCategorizeTransactionsR = do
    app <- getYesod
    let openaiKey = appOpenAiKey $ appSettings app

    csvBS <- rawRequestBody C.$$ CL.fold BS.append BS.empty
    let bankType = detectBankType csvBS
    let result = case bankType of
                ChaseBank -> chaseHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                WellsFargoBank -> wfHandler $ Csv.decode Csv.NoHeader $ LBS.fromStrict csvBS
                UnknownBank -> error "Unknown bank"
    recategorizedTransactions <- liftIO $ recategorizeTransactions openaiKey result
    returnJson recategorizedTransactions
    where
        chaseHandler :: Either String (Vector Chase.ChaseTransaction) -> Vector Transaction
        chaseHandler (Left _) = error "Failed to parse Chase csv"
        chaseHandler (Right transactions) = Chase.toTransaction <$> transactions

        wfHandler :: Either String (Vector WellsFargo.WellsFargoTransaction) -> Vector Transaction
        wfHandler (Left _) = error "Failed to parse Wells Fargo csv"
        wfHandler (Right transactions) = WellsFargo.toTransaction <$> transactions


recategorizeTransactions :: Text -> Vector Transaction -> IO (Vector Transaction)
recategorizeTransactions openaiKey transactions = do
    -- must parse this into a maybe
    categories <- categorizeTransactions openaiKey $ toList (description <$> transactions)
    let categorizedTransactions = fmap createCategorizedTransactions (V.zip transactions categories)

    pure categorizedTransactions
    where
        createCategorizedTransactions :: (Transaction, Text) -> Transaction
        createCategorizedTransactions (transaction, category) = updateTransactionCategory transaction category