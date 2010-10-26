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
-- Any function here that returns a value might also throw a NotFound, AccessForbidden, or OtherError
-- if Twitter gives it the appropriate error (HTTP 404, HTTP 401, or anything else)
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
         getStatus,
         authGetStatus,
         Status(..),
         TwitterException(..)
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

-- | A type representing a single status update, or 'tweet'.
data Status = Status {
    user :: String, -- ^ The username of the poster of the status.
    text :: String  -- ^ The content of the status update.
    } deriving (Eq, Show)

-- | A type representing an error that happened while doing something Twitter-related.
data TwitterException
     = NotFound        -- ^ The requested object was not found.
     | AccessForbidden -- ^ You do not have permission to access the requested entity.
     | OtherError      -- ^ Something else went wrong.
     deriving (Eq, Show, Typeable)
instance Exception TwitterException

makeStatus :: JSObject JSValue -> Result Status
makeStatus tweet = do
    userObject <- valFromObj "user" tweet
    user <- valFromObj "screen_name" userObject
    text <- valFromObj "text" tweet
    return Status {user = user, text = text}

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
    mapM makeStatus tweets

updateStatus :: Token -> String -> IO Response
updateStatus token status = unwrap $ do
    putToken token
    doRequest POST "statuses/update"  [("status", status)]
    
publicTimeline :: IO [Status]
publicTimeline  = fmap parseTimeline . unwrap $ doRequest GET "statuses/public_timeline" []

homeTimeline :: Token -> IO [Status]
homeTimeline token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/home_timeline" []

friendsTimeline :: Token -> IO [Status]
friendsTimeline token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/friends_timeline" []

userTimeline :: String -> IO [Status]
userTimeline name = fmap parseTimeline . unwrap $
    doRequest GET "statuses/user_timeline" [("screen_name", name)]

authUserTimeline :: Token -> String -> IO [Status]
authUserTimeline token name = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/user_timeline" [("screen_name", name)]

mentions :: Token -> IO [Status]
mentions token = fmap parseTimeline . unwrap $ do
    putToken token
    doRequest GET "statuses/mentions" []

getStatus :: String -> IO Status
getStatus tweetId = unwrap $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> makeStatus) $ rsp

authGetStatus :: Token -> String -> IO Status
authGetStatus token tweetId = unwrap $ do
    putToken token
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> makeStatus) $ rsp