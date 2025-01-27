module Main where

import Utils.Csv

main :: IO ()
main = do
    _ <- readCSV "chase-example.csv"
    pure ()