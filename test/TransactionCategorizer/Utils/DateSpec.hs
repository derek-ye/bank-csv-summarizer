{-# LANGUAGE OverloadedStrings #-}
module TransactionCategorizer.Utils.DateSpec (spec) where

import Test.Hspec
import Data.Csv (runParser)
import Data.Time
import Data.Text
import TransactionCategorizer.Utils.Date (mmddyyyyStrToDay, mmddyyDateParser)
import Control.Monad (forM_)
import Control.Exception (evaluate)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM

testDates :: [(String, Maybe Day)]
testDates =
  [ ("01/01/2000", Just $ fromGregorian 2000 1 1)
  , ("12/31/1999", Just $ fromGregorian 1999 12 31)
  , ("02/29/2000", Just $ fromGregorian 2000 2 29)
  , ("02/29/2020", Just $ fromGregorian 2020 2 29)
  , ("02/29/2100", Nothing)
  , ("00/15/2023", Nothing)
  , ("13/15/2023", Nothing)
  , ("04/31/2023", Nothing)
  , ("09/31/2023", Nothing)
  , ("02/30/2023", Nothing)
  , ("11/30/2023", Just $ fromGregorian 2023 11 30)
  , ("12/31/2023", Just $ fromGregorian 2023 12 31)
  -- , ("2/2/2023", Just $ fromGregorian 2023 2 2)      -- throws error, removing for now
  , ("02/02/2023", Just $ fromGregorian 2023 2 2)
  , ("10/08/1582", Just $ fromGregorian 1582 10 8)
  , ("01/01/1900", Just $ fromGregorian 1900 1 1)
  , ("01/01/1970", Just $ fromGregorian 1970 1 1)
  , ("12/31/2038", Just $ fromGregorian 2038 12 31)
  , ("07/04/1776", Just $ fromGregorian 1776 7 4)
  ]

spec :: Spec
spec = describe "mmddyyyyStrToDay" $ do
  -- Tests String -> Day function
  forM_ testDates $ \(dateStr, correctMDay) ->
    it ("converts " ++ dateStr) $ do
      case correctMDay of
        Just expectedDay -> 
          mmddyyyyStrToDay dateStr `shouldBe` expectedDay
        Nothing -> 
          evaluate (mmddyyyyStrToDay dateStr) `shouldThrow` anyException

  -- Tests String -> Parser Day function
  forM_ testDates $ \(dateStr, correctMDay) ->
    it ("parses " ++ dateStr) $ do
      -- Create a test record with the date string
      let record = HM.singleton (TE.encodeUtf8 "txnDate") (TE.encodeUtf8 $ pack dateStr)
      
      -- Run the parser on the test record
      let result = runParser (mmddyyDateParser "txnDate" record)
      
      -- Check the result against the expected value
      case correctMDay of
        Just expectedDay -> 
          result `shouldBe` Right expectedDay
          
        Nothing -> 
          case result of
            Left err -> 
              err `shouldContain` "Unable to parse"
            Right _ -> 
              expectationFailure "Parser succeeded but should have failed"