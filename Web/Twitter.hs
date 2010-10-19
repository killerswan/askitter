module Web.Twitter (
    updateStatus
    ) where

import Data.Maybe (fromJust)
import Web.Twitter.OAuth
import Control.Monad.Trans (liftIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    let request = url { method = POST, qString = fromList [("status", status)]}
    serviceRequest HMACSHA1 Nothing request
    where url = fromJust . parseURL $ "http://api.twitter.com/1/statuses/update.json"