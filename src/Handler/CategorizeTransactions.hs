{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.CategorizeTransactions where

import Import hiding ((.), zip)
import qualified Data.Map as Map
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import TransactionCategorizer.BankParsers.Transaction
import qualified TransactionCategorizer.BankParsers.Chase as Chase
import qualified TransactionCategorizer.BankParsers.WellsFargo as WellsFargo
import qualified TransactionCategorizer.BankParsers.Citi as Citi
import TransactionCategorizer.Core.Categorizer (categorizeTransactions)
import qualified Data.Vector as V
import TransactionCategorizer.Utils.Csv
import Network.Wai
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)

newtype CategorizeTransactionsResult = CategorizeTransactionsResult {
    categorizedTransactions :: Map.Map Text Text
} deriving (Generic, Show)

instance ToJSON CategorizeTransactionsResult

postCategorizeTransactionsR :: Handler TypedContent
postCategorizeTransactionsR = do
    app <- getYesod
    let openaiKey = appOpenAiKey $ appSettings app

    -- Get raw request body as ByteString
    req <- waiRequest
    rawBody <- liftIO $ Network.Wai.requestBody req
    let csvBS = rawBody

    let bankType = detectBankType csvBS
    let result = case bankType of
                ChaseBank -> chaseHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                CitiBank -> citiHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                CapitalOneBank -> error "Cannot currently parse Capital One CSVs - check back later!" -- capitalOneHandler $ removeHeader $ Csv.decodeByName $ LBS.fromStrict csvBS
                WellsFargoBank -> wfHandler $ Csv.decode Csv.NoHeader $ LBS.fromStrict csvBS
                UnknownBank -> error "Unknown bank"
    recategorizedTransactions <- liftIO $ recategorizeTransactions openaiKey result
    let csv = toCsv recategorizedTransactions
    pure $ TypedContent "text/csv" $ toContent csv
    where
        chaseHandler :: Either String (Vector Chase.ChaseTransaction) -> Vector Transaction
        chaseHandler (Left e) = error $ "Failed to parse Chase csv: " <> e
        chaseHandler (Right transactions) = Chase.toTransaction <$> transactions

        wfHandler :: Either String (Vector WellsFargo.WellsFargoTransaction) -> Vector Transaction
        wfHandler (Left _) = error "Failed to parse Wells Fargo csv"
        wfHandler (Right transactions) = WellsFargo.toTransaction <$> transactions

        citiHandler :: Either String (Vector Citi.CitiTransaction) -> Vector Transaction
        citiHandler (Left e) = error $ "Failed to parse Citi csv: "  <> e
        citiHandler (Right transactions) = Citi.toTransaction <$> transactions


recategorizeTransactions :: Text -> Vector Transaction -> IO (Vector Transaction)
recategorizeTransactions openaiKey transactions = do
    -- must parse this into a maybe
    categories <- categorizeTransactions openaiKey $ toList (description <$> transactions)

    pure $ fmap createCategorizedTransactions (V.zip transactions categories)
    where
        createCategorizedTransactions :: (Transaction, Text) -> Transaction
        createCategorizedTransactions (t, c) = updateTransactionCategory t c
