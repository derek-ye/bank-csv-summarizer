{-# LANGUAGE OverloadedStrings #-}
module Core.CategorizerSpec where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS8
import TransactionCategorizer.BankParsers.Transaction (BankType(..), detectBankType)
import TransactionCategorizer.Core.Categorizer

transactionDescriptions = ["PIXEL PLANET"
                          ,"HANDYMAN’S PARADISE"
                          ,"FUEL GALAXY"
                          ]

-- Rework these tests
--
spec :: Spec
spec = describe "categorizeTransactions" $ do
            it "dummy" $ do
                True
-- spec = describe "categorizeTransactions" $ do
--     it "accurately categorizes 1 transaction description" $ do
--         categorizeTransaction  [head transactionDescriptions]
--     it "accurately categorizes many transaction descriptions" $ do
--         categorizeTransactions transactionDescriptions