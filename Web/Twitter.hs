module Web.Twitter
       ( updateStatus
       , publicTimeline
       ) where

import Data.Maybe (fromJust)
import Web.Twitter.OAuth
import Control.Monad.Trans (liftIO, MonadIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient

buildRequest ::  Method -> String -> [(String, String)] -> Request
buildRequest method part query =
    (fromJust . parseURL $ "http://api.twitter.com/1/" ++ part ++ ".json") { method = method, qString = fromList query}

doRequest :: (MonadIO m, HttpClient m) => Method -> String -> [(String, String)] -> OAuthMonad m Response
doRequest meth part query = serviceRequest HMACSHA1 Nothing $ buildRequest meth part query

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    doRequest POST "statuses/update"  [("status", status)]

publicTimeline :: IO Response
publicTimeline  = unwrap $ doRequest GET "statuses/public_timeline" []

homeTimeline :: Token -> IO Response
homeTimeline token = unwrap $ do
    putToken token
    doRequest GET "statuses/home_timeline" []
