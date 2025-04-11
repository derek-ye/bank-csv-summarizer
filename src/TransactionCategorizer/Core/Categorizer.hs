{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module TransactionCategorizer.Core.Categorizer where

import OpenAI.V1
import OpenAI.V1.Chat.Completions
import qualified Data.Text as T
import qualified Data.Vector as V

categorizeTransaction :: T.Text -> T.Text -> IO (V.Vector T.Text)
categorizeTransaction key transactionText = do

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv key

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text = promptText } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        , temperature = Just 0  -- we want there to be as little variance as possible between predictions
        }

    pure $ chatCompletionToTextArr choices

    where
        promptText = "You are a high-performant system that banks use to categorize credit and debit card transactions. Given the options [Food & drink, Entertainment, General merchandise, General services, Payment, Personal care, Rent & utilities, Transportation, Travel], please categorize this transaction concisely: '" <> transactionText <> "'"
        chatCompletionToTextArr :: V.Vector Choice -> V.Vector T.Text
        chatCompletionToTextArr choices = messageToContent . message <$> choices

categorizeTransactions :: T.Text -> [T.Text] -> IO (V.Vector T.Text)
categorizeTransactions key transactions = do

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv key

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text = promptText } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        , temperature = Just 0  -- we want there to be as little variance as possible between predictions
        }

    pure $ chatCompletionToTextArr choices

    where
        promptText = "You are a high-performant system that banks use to categorize credit and debit card transactions. Given the options [Food & drink, Entertainment, General merchandise, General services, Payment, Personal care, Rent & utilities, Transportation, Travel], please categorize this list of transactions and return it as a single word: '" <> commaJoin transactions <> "'"
        chatCompletionToTextArr :: V.Vector Choice -> V.Vector T.Text
        chatCompletionToTextArr choices = messageToContent . message <$> choices
        commaJoin = T.intercalate ","
