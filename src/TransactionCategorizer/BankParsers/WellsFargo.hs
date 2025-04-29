module TransactionCategorizer.BankParsers.WellsFargo where
import qualified Data.Text as T
import Data.Csv ( (.!), FromRecord(..) )
import qualified TransactionCategorizer.BankParsers.Transaction as Trans
import TransactionCategorizer.Utils.Date

data WellsFargoTransaction = MkWellsFargoTransaction {
    transactionDate :: T.Text,  -- leave as text for now
    amount :: Double,
    asterisk :: T.Text,
    memo :: T.Text,
    description :: T.Text
} deriving (Show)

instance FromRecord WellsFargoTransaction where
    parseRecord v
        | length v == 5 = MkWellsFargoTransaction <$>
                          v .! 0 <*>
                          v .! 1 <*>
                          v .! 2 <*>
                          v .! 3 <*>
                          v .! 4
        | otherwise     = mempty

toTransaction :: WellsFargoTransaction -> Trans.Transaction
toTransaction MkWellsFargoTransaction { transactionDate=wfTransactionDate
                                        , amount=wfAmount
                                        , asterisk=_
                                        , memo=_
                                        , description=wfDescription
                                    } = Trans.MkTransaction { Trans.transactionDate=mmddyyyyStrToDay $ T.unpack wfTransactionDate, Trans.description=wfDescription, Trans.category=Nothing, Trans.amount= - wfAmount }
