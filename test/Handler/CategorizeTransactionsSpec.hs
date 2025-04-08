module Handler.CategorizeTransactionsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "postCategorizeTransactionsR" $ do
        it "return true" $ do
            get HomeR
            statusIs 200

