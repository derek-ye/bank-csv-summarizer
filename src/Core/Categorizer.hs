{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Core.Categorizer where

import Data.Foldable (traverse_)
import OpenAI.V1
import OpenAI.V1.Chat.Completions

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import Configuration.Dotenv

main2 :: IO ()
main2 = do
    -- loads .env file into environment
    loadFile defaultConfig
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    let text = "You are a high-performant system that banks use to categorize credit and debit card transactions. Given the options [Food & drink, Entertainment, General merchandise, General services, Payment, Personal care, Rent & utilities, Transportation, Travel], please categorize this transaction concisely: 'CYBERSPACE SUBSCRIPTION'"
    -- text <- Text.IO.getLine

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        , temperature = Just 0
        }

    let display Choice{ message } = Text.IO.putStrLn (messageToContent message)

    traverse_ display choices