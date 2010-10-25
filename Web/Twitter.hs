{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.Twitter
       ( updateStatus
       , publicTimeline
       ) where

import Data.Maybe (fromJust)
import Web.Twitter.OAuth
import Control.Monad
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.HttpClient
import Text.JSON
import qualified Data.ByteString.Lazy.Char8 as L8

data Tweet = Tweet { user :: String
                   , text :: String
                   } deriving (Eq, Show)

makeTweet :: JSObject JSValue -> Tweet
makeTweet tweet = Tweet { user = unResult $ valFromObj "user" tweet >>= valFromObj "screen_name"
                        , text = unResult $ valFromObj "text" tweet}
                  

makeJson :: (JSON a) => Response -> Result a
makeJson = decode . L8.unpack . rspPayload

unResult = \(Ok x) -> x

buildRequest ::  Method -> String -> [(String, String)] -> Request
buildRequest method part query =
    (fromJust . parseURL $ "http://api.twitter.com/1/" ++ part ++ ".json") { method = method, qString = fromList query}

doRequest :: (MonadIO m, HttpClient m) => Method -> String -> [(String, String)] -> OAuthMonad m Response
doRequest meth part query = serviceRequest HMACSHA1 Nothing $ buildRequest meth part query

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    doRequest POST "statuses/update"  [("status", status)]

parseTimeline :: Response -> [Tweet]
parseTimeline rsp = unResult $ do
    json <- makeJson rsp
    tweets <- (readJSONs json :: Result [JSValue]) >>= mapM readJSON
    return $ makeTweet <$> tweets



publicTimeline :: IO [Tweet]
publicTimeline  = fmap parseTimeline . unwrap $ doRequest GET "statuses/public_timeline" []

homeTimeline :: Token -> IO [Tweet]
homeTimeline token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/home_timeline" []

friendsTimeline :: Token -> IO [Tweet]
friendsTimeline token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/friends_timeline" []

authUserTimeline :: Token -> String -> IO [Tweet]
authUserTimeline token name = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/user_timeline" [("screen_name", name)]

userTimeline :: String -> IO [Tweet]
userTimeline name = fmap parseTimeline . unwrap $ do
    doRequest GET "statuses/user_timeline" [("screen_name", name)]
    
mentions :: Token -> IO [Tweet]
mentions token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/mentions" []

getTweet :: String -> IO Tweet
getTweet tweetId = unwrap $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . makeTweet . unResult . makeJson $ rsp

authGetTweet :: Token -> String -> IO Tweet
authGetTweet token tweetId = unwrap $ do
    putToken token
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . makeTweet . unResult . makeJson $ rsp
