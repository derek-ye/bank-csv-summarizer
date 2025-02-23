module TransactionCategorizer.BankParserSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS8
import TransactionCategorizer.BankParsers.Transaction (BankType(..), detectBankType)

chaseCsv = "Transaction Date,Post Date,Description,Category,Type,Amount,Memo\n06/03/2023,06/04/2023,PIXEL PLANET,Gaming,Debit,-59.99,"
wfCsv = "\"11/08/2023\",\"-15.20\",\"*\",\"\",\"SIP & SAVOR - SEATTLE\""

spec :: Spec
spec = describe "detectBankType" $ do
    it "accurately detects Chase csvs" $ do
        (detectBankType $ BS8.pack chaseCsv) `shouldBe` ChaseBank
    it "accurately detects Wells Fargo csvs" $ do
        (detectBankType $ BS8.pack wfCsv) `shouldBe` WellsFargoBank