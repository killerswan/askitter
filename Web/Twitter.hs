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
{-       ( updateStatus,
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
-} where

import Data.Maybe (fromJust)
import Web.Twitter.OAuth
import Control.Monad
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO, MonadIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Text.JSON
import Control.Exception
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy.Char8 as L8

-- | A type representing a single status update, or 'tweet'.
data Status = Status {
    user :: String, -- ^ The username of the poster of the status.
    text :: String  -- ^ The content of the status update.
    } deriving (Eq, Show)

instance JSON Status where
    readJSON (JSObject tweet) = let (!) = flip valFromObj in do
        userObject <- tweet ! "user"
        user <- userObject ! "screen_name"
        text <- tweet ! "text"
        return Status {user = user, text = text}
    showJSON = undefined

-- | A type representing an error that happened while doing something Twitter-related.
data TwitterException
     = NotFound        -- ^ The requested object was not found.
     | AccessForbidden -- ^ You do not have permission to access the requested entity.
     | OtherError      -- ^ Something else went wrong.
     deriving (Eq, Show, Typeable)
instance Exception TwitterException

makeJSON :: (JSON a) => Response -> Result a
makeJSON = decode . L8.unpack . rspPayload

buildRequest ::  Method -> String -> [(String, String)] -> Request
buildRequest method part query =
    (fromJust . parseURL $ "http://api.twitter.com/1/" ++ part ++ ".json") { method = method, qString = fromList query}

doRequest :: Method -> String -> [(String, String)] -> OAuthMonadT IO Response
doRequest meth part query = signRq2 HMACSHA1 Nothing (buildRequest meth part query) >>= serviceRequest CurlClient

withoutAuth :: (Monad m) => OAuthMonadT m a -> m a
withoutAuth = runOAuthM (fromApplication $ Application "" "" OOB)

-- Run the parser on the given response, but throw the appropriate
-- error given by the HTTP error code if parsing fails
handleErrors :: (Response -> Result a) -> Response -> a
handleErrors parser rsp = case parser rsp of
    Ok parsed -> parsed
    Error _ -> case status rsp of 
        401 -> throw AccessForbidden
        404 -> throw NotFound
        _   -> throw OtherError

-- Take a timeline response and turn it into a list of Statuses.
parseTimeline :: Response -> [Status]
parseTimeline = handleErrors $ makeJSON >=> readJSON

-- | Update the authenticating user's timeline with the given status
-- string. Returns IO () always, but doesn't do any exception
-- handling. Someday I'll fix that.
updateStatus :: Token -> String -> IO ()
updateStatus token status = runOAuthM token $ do
    doRequest POST "statuses/update"  [("status", status)]
    return ()

-- | Get the public timeline as a list of statuses.
publicTimeline :: IO [Status]
publicTimeline  = fmap parseTimeline . withoutAuth $ doRequest GET "statuses/public_timeline" []

-- | Get the last 20 updates of the authenticating user's home
-- timeline, meaning all their statuses and those of their
-- friends. Will throw an @AccessForbidden@ if your token is invalid.
homeTimeline :: Token -> IO [Status]
homeTimeline token = fmap parseTimeline . runOAuthM token $ doRequest GET "statuses/home_timeline" []

-- | Get the authenticating user's friends timeline. This is the same
-- as their home timeline, except it excludes RTs by default. Note
-- that if 5 of the last 20 tweets were RTs, this will only return 15
-- statuses.
friendsTimeline :: Token -> IO [Status]
friendsTimeline token = fmap parseTimeline . runOAuthM token $ do
    putToken token
    doRequest GET "statuses/friends_timeline" []

-- | Get the last 20 tweets, without RTs, of the given username,
-- without authentication. If this throws an @AccessForbidden@ error,
-- the user's timeline is protected.
userTimeline :: String -> IO [Status]
userTimeline name = fmap parseTimeline . withoutAuth $ doRequest GET "statuses/user_timeline" [("screen_name", name)]

-- | Get the last 20 updates, without RTs, of the given username, with
-- authentication. If this throws an @AccessForbidden@ error, their
-- timeline is protected and you aren't allowed to see it.
authUserTimeline :: Token -> String -> IO [Status]
authUserTimeline token name = fmap parseTimeline . runOAuthM token $ doRequest GET "statuses/user_timeline" [("screen_name", name)]

-- | Get the last 20 mentions of the authenticating user.
mentions :: Token -> IO [Status]
mentions token = fmap parseTimeline . runOAuthM token $ doRequest GET "statuses/mentions" []

-- | Get a @Status@ corresponding to the given id, without authentication.
getStatus :: String -> IO Status
getStatus tweetId = withoutAuth $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> readJSON) $ rsp

-- | Get a @Status@ corresponding to the given id, with authentication
authGetStatus :: Token -> String -> IO Status
authGetStatus token tweetId = runOAuthM token $ do
    rsp <- doRequest GET ("statuses/show/" ++ tweetId) []
    return . handleErrors (makeJSON >=> readJSON) $ rsp
    