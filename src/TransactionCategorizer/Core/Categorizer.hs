{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module TransactionCategorizer.Core.Categorizer where

import Data.Foldable (traverse_)
import OpenAI.V1
import OpenAI.V1.Chat.Completions

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import Configuration.Dotenv
import qualified Data.Text as T
import qualified Data.Vector as V

main2 :: IO ()
main2 = do
    responseVector <- categorizeTransaction "CYBERSPACE SUBSCRIPTION"
    -- let display Choice{ message } = Text.IO.putStrLn (messageToContent message)
    traverse_ Text.IO.putStrLn responseVector
    

-- modularize this so other libraries / code can import it
categorizeTransaction :: T.Text -> IO (V.Vector T.Text)
categorizeTransaction transactionText = do
    -- loads .env file into environment
    loadFile defaultConfig
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

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

categorizeTransactions :: [T.Text] -> IO (V.Vector T.Text)
categorizeTransactions transactions = do
    -- loads .env file into environment
    loadFile defaultConfig
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text = promptText } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        , temperature = Just 0  -- we want there to be as little variance as possible between predictions
        }

    pure $ chatCompletionToTextArr choices

    where
        promptText = "You are a high-performant system that banks use to categorize credit and debit card transactions. Given the options [Food & drink, Entertainment, General merchandise, General services, Payment, Personal care, Rent & utilities, Transportation, Travel], please categorize this list of transactions and return in this format: {'description':'category'}. '" <> commaJoin transactions <> "'"
        chatCompletionToTextArr :: V.Vector Choice -> V.Vector T.Text
        chatCompletionToTextArr choices = messageToContent . message <$> choices
        commaJoin = T.intercalate ","
