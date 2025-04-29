{-# LANGUAGE OverloadedStrings #-}

module TransactionCategorizer.Utils.Date where

import Data.Time
import Data.Text hiding (length)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Csv
    ( (.:), Parser, NamedRecord )
import Text.Read (readMaybe)

mmddyyyyStrToDay :: String -> Day
mmddyyyyStrToDay str = 
  case parseTimeM True defaultTimeLocale "%m/%d/%Y" str of
    Just date -> date
    Nothing -> error $ "Failed to parse date: " ++ str

mmddyyyyDateParser :: Text -> NamedRecord -> Parser Day
mmddyyyyDateParser fieldName r = do
    let fieldNameBS = TE.encodeUtf8 fieldName
    txnDate <- r .: fieldNameBS     -- txnDate is a date string that should look like '07/16/2023'
    dateTriple <- case dateStringToIntArray txnDate of
                                Just date -> pure date
                                Nothing -> fail "Unable to parse MMDDYYYY date field"
    let (month, day, year) = dateTriple
    pure $ fromGregorian (fromIntegral year) (fromIntegral month) (fromIntegral day)
  where
    dateStringToIntArray :: String -> Maybe (Int, Int, Int)
    dateStringToIntArray dateStr = do
      let textArr = splitOn "/" (pack dateStr)
      if length textArr == 3
        then case TR.decimal <$> textArr of
                  [Right (m, _), Right (d, _), Right (y, _)] -> Just (m, d, y)
                  _ -> Nothing
        else Nothing

yyyymmddSkewerDateParser :: Text -> NamedRecord -> Parser Day
yyyymmddSkewerDateParser fieldName r = do
    let fieldNameBS = TE.encodeUtf8 fieldName
    value <- r .: fieldNameBS
    case splitOn "-" value of
        [yearStr, monthStr, dayStr] -> do
            year <- parseYear yearStr
            month <- parseMonth monthStr
            day <- parseDay dayStr
            case fromGregorianValid year month day of
                Just date -> return date
                Nothing -> fail $ "Invalid date: " <> unpack value
        _ -> fail $ "Date must be in YYYY-MM-DD format: " <> unpack value
  where
    parseYear str = case readMaybe (unpack str) of
        Just y -> return y
        Nothing -> fail $ "Invalid year: " <> unpack str
    
    parseMonth str = case readMaybe (unpack str) of
        Just m | m >= 1 && m <= 12 -> return m
        _ -> fail $ "Invalid month: " <> unpack str
    
    parseDay str = case readMaybe (unpack str) of
        Just d | d >= 1 && d <= 31 -> return d
        _ -> fail $ "Invalid day: " <> unpack str
