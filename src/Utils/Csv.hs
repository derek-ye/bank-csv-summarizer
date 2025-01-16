{-# LANGUAGE OverloadedStrings #-}
module Utils.Csv where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Csv
    ( decodeByName )
import BankParsers.Chase (ChaseTransaction(..))

readCSV :: FilePath -> IO ()
readCSV fileName = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> putStrLn "wtf"
        Right (_, v) -> V.forM_ v $ \ txn ->
            putStrLn (T.unpack $ description txn <> " - " <> T.show (transactionDate txn))