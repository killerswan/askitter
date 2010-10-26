{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}
module Web.Twitter
       ( updateStatus
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
import Control.Exception
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8

data Tweet = Tweet { user :: String
                   , text :: String
                   } deriving (Eq, Show)

data TwitterException = NotFound | AccessForbidden | OtherError deriving (Eq, Show, Typeable)
instance Exception TwitterException

parseError = undefined

makeTweet :: JSObject JSValue -> Result Tweet
makeTweet tweet = do
    userObject <- valFromObj "user" tweet
    user <- valFromObj "screen_name" userObject
    text <- valFromObj "text" tweet
    return Tweet {user = user, text = text}
                  

makeJSON :: (JSON a) => Response -> Result a
makeJSON = decode . L8.unpack . rspPayload

buildRequest ::  Method -> String -> [(String, String)] -> Request
buildRequest method part query =
    (fromJust . parseURL $ "http://api.twitter.com/1/" ++ part ++ ".json") { method = method, qString = fromList query}

doRequest :: (MonadIO m, HttpClient m) => Method -> String -> [(String, String)] -> OAuthMonad m Response
doRequest meth part query = serviceRequest HMACSHA1 Nothing $ buildRequest meth part query

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    doRequest POST "statuses/update"  [("status", status)]

handleErrors parser rsp = case (parser rsp) of
    Ok parsed -> parsed
    Error _ -> case status rsp of 
        401 -> throw AccessForbidden
        404 -> throw NotFound
        _   -> throw OtherError

parseTimeline = handleErrors $ \rsp -> do 
    json <- makeJSON rsp
    tweets <- readJSONs json >>= mapM readJSON
    sequence $ map makeTweet tweets


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
{-
getTweet :: String -> IO Tweet
getTweet tweetId = unwrap $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . makeTweet . (handleErrors makeJSON) $ rsp

authGetTweet :: Token -> String -> IO Tweet
authGetTweet token tweetId = unwrap $ do
    putToken token
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . makeTweet . (handleErrors makeJSON) $ rsp
-}