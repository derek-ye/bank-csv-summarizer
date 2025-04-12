{-# OPTIONS_GHC -fno-warn-orphans #-}
module TransactionCategorizer.BankParsers.Other.Day where

import Data.Time.Calendar (Day(..))
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Csv (FromField(..))
import qualified Data.ByteString.Char8 as BS8

instance FromField Day where
    parseField s = do
        -- Adjust the format string based on your CSV date format
        -- This example assumes "YYYY-MM-DD"
        case parseTimeM True defaultTimeLocale "%m-%d-%Y" (BS8.unpack s) of
            Just day -> pure day
            Nothing -> fail $ "could not parse date: " ++ show s
