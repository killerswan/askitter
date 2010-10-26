----------------------------------------------------
-- |
-- Module: Web.Twitter
-- Description: OAuth Twitter bindings
-- License: MIT
-- Maintainer: Patrick Hurst <phurst@mit.edu>
-- Stability: experimental
-- Portability: portable
--
-- Twitter bindings that use OAuth instead of basic authentication.
--
-----------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

module Web.Twitter
       ( updateStatus,
         publicTimeline,
         homeTimeline,
         friendsTimeline,
         authUserTimeline,
         userTimeline,
         mentions,
         getTweet,
         authGetTweet
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

handleErrors parser rsp = case (parser rsp) of
    Ok parsed -> parsed
    Error _ -> case status rsp of 
        401 -> throw AccessForbidden
        404 -> throw NotFound
        _   -> throw OtherError

parseTimeline = handleErrors $ \rsp -> do 
    json <- makeJSON rsp
    tweets <- readJSONs json >>= mapM readJSON
    mapM makeTweet tweets

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    doRequest POST "statuses/update"  [("status", status)]
    
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

userTimeline :: String -> IO [Tweet]
userTimeline name = fmap parseTimeline . unwrap $
    doRequest GET "statuses/user_timeline" [("screen_name", name)]

authUserTimeline :: Token -> String -> IO [Tweet]
authUserTimeline token name = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/user_timeline" [("screen_name", name)]

mentions :: Token -> IO [Tweet]
mentions token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/mentions" []

getTweet :: String -> IO Tweet
getTweet tweetId = unwrap $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> makeTweet) $ rsp

authGetTweet :: Token -> String -> IO Tweet
authGetTweet token tweetId = unwrap $ do
    putToken token
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> makeTweet) $ rsp