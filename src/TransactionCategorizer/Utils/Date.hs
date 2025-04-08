{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.Utils.Date where

import Data.Time
import qualified Data.Text as T

parseDate :: String -> Day
parseDate str = 
  case parseTimeM True defaultTimeLocale "%m/%d/%Y" str of
    Just date -> date
    Nothing -> error $ "Failed to parse date: " ++ str