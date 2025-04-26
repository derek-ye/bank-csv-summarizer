{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.Utils.Date where

import Data.Time
import Data.Text hiding (length)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Csv
    ( (.:), Parser, NamedRecord )

mmddyyyyStrToDay :: String -> Day
mmddyyyyStrToDay str = 
  case parseTimeM True defaultTimeLocale "%m/%d/%Y" str of
    Just date -> date
    Nothing -> error $ "Failed to parse date: " ++ str

mmddyyyyDateParser :: Text -> NamedRecord -> Parser Day
mmddyyyyDateParser fieldName r = do
    let fieldNameBS = TE.encodeUtf8 fieldName
    txnDate <- r .: fieldNameBS     -- txnDate is a date string that should look like '07/16/2023'
    dateTriple <- case citiDateStringToIntArray txnDate of
                                Just date -> pure date
                                Nothing -> fail "Unable to parse Chase date field"
    let (month, day, year) = dateTriple
    pure $ fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
  where
    citiDateStringToIntArray :: String -> Maybe (Int, Int, Int)
    citiDateStringToIntArray dateStr = do
      let textArr = splitOn "/" (pack dateStr)
      if length textArr == 3
        then case TR.decimal <$> textArr of
                  [Right (m, _), Right (d, _), Right (y, _)] -> Just (m, d, y)
                  _ -> Nothing
        else Nothing