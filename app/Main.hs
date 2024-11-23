module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Csv

data Transaction = ChaseTransaction

data ChaseTransaction = MkChaseTransaction {
    transactionDate :: UTCTime,
    postDate :: UTCTime,
    description :: Text,
    category :: Text,
    transactionType :: ChaseCardTransactionType,
    amount :: Double,
    memo :: Text
} deriving (Show)

instance FromNamedRecord ChaseTransaction
instance ToNamedRecord ChaseTransaction
instance DefaultOrdered ChaseTransaction

data ChaseCardTransactionType = Credit | Debit deriving (Show)

main :: IO ()
main = do
    result <- readCSV "chase-example.csv"
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right (_, v) -> V.forM_ v print

readCSV :: FilePath -> IO (Either String (V.Vector Transaction))
readCSV fileName = do
    csvData <- BL.readFile fileName
    return $ decodeByName csvData
