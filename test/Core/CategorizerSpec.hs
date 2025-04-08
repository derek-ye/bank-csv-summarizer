module Core.CategorizerSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS8
import TransactionCategorizer.BankParsers.Transaction (BankType(..), detectBankType)

transactionDescriptions = ["PIXEL PLANET"
                          ,"HANDYMAN’S PARADISE"
                          ,"FUEL GALAXY"
                          ]

spec :: Spec
spec = describe "categorizeTransactions" $ do
    it "accurately categorizes 1 transaction description" $ do
        categorizeTransaction $ transactionDescription !! 0
    it "accurately categorizes many transaction descriptions" $ do
        categorizeTransactions transactionDescriptions