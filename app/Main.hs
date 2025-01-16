module Main where

import Utils.Csv

main :: IO ()
main = do
    result <- readCSV "chase-example.csv"
    pure ()