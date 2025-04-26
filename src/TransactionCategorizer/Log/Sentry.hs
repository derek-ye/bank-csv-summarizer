module TransactionCategorizer.Log.Sentry where

import System.Log.Raven
import System.Log.Raven.Types (SentryLevel(..))
import System.Log.Raven.Transport.HttpConduit (sendRecordWith)
import qualified Network.HTTP.Conduit as HTTP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

logToSentry :: T.Text -> String -> IO ()
logToSentry sentryDsn e = do
    http <- HTTP.newManager HTTP.tlsManagerSettings
    l <- initRaven
        (T.unpack sentryDsn)
        id
        (sendRecordWith http)
        stderrFallback
    register l "my.logger.name" Error e id