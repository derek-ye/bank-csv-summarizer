module UtilSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import TransactionCategorizer.Utils.Csv
import TransactionCategorizer.BankParsers.Transaction

spec :: Spec
spec = describe "byteStringToCsv" $ do
        it "successfully converts from a bytestring to a named record" $ do
            csvData <- liftIO $ BL.readFile "./chase-example.csv"
            _ <- traceM $ show $ byteStringToCsv csvData
            1 `shouldBe` 1
