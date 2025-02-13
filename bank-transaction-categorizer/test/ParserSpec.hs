module ParserSpec (spec) where
import qualified Data.ByteString.Lazy as BL
import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import TransactionCategorizer.Utils.Csv
import TransactionCategorizer.BankParsers.Chase
import TransactionCategorizer.BankParsers.WellsFargo
import Data.Csv
    ( decode, decodeByName, HasHeader(..), Header )
import qualified Data.Vector as V

spec :: Spec
spec = describe "parser" $ do
            it "parse chase" $ do
                csvData <- liftIO $ BL.readFile "./chase-example.csv"
                let decoded :: Either String (Header, V.Vector ChaseTransaction)
                    decoded = decodeByName csvData
                either 
                    (fail . ("Chase CSV parse failed: " ++))  -- handle Left case
                    (\(_, csv) -> do                         -- handle Right case
                        _ <- traceM $ show csv
                        1 `shouldBe` 1
                    )
                    decoded
            it "parse wellsfargo" $ do
                csvData <- liftIO $ BL.readFile "./wf-example.csv"
                let decoded :: Either String (V.Vector WellsFargoTransaction)
                    decoded = decode NoHeader csvData
                either 
                    (fail . ("WF CSV parse failed: " ++))  -- handle Left case
                    (\csv -> do                         -- handle Right case
                        _ <- traceM $ show csv
                        1 `shouldBe` 1
                    )
                    decoded