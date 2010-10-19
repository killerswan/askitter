module Web.Twitter.OAuth
       ( getAuthenticateURL
       , makeToken
       , Consumer
       ) where

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (parseURL, findWithDefault)
import Network.OAuth.Http.HttpClient (HttpClient)


reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"

tweetUrl = fromJust . parseURL $ "http://api.twitter.com/1/statuses/update.json"

authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
            . findWithDefault ("oauth_token","") . oauthParams

data Consumer = Consumer
    { key :: String
    , secret :: String }
    deriving (Show, Eq)

getAuthenticateURL :: (HttpClient m, MonadIO m) => Consumer -> OAuthMonad m String
getAuthenticateURL consumer = do
    ignite $ Application (key consumer) (secret consumer) OOB 
    oauthRequest HMACSHA1 Nothing reqUrl
    authUrl <$> getToken

makeToken :: (HttpClient m, MonadIO m) => String -> OAuthMonad m Token
makeToken answer = do
    token <- getToken
    putToken $ injectOAuthVerifier answer token
    oauthRequest HMACSHA1 Nothing accUrl
    getToken

{-
Sample code:
authenticate :: Consumer -> IO Token
authenticate consumer = unCurlM . runOAuth $ do
    ignite $ Application (key consumer) (secret consumer) OOB 
    url <- getAuthenticateURL consumer
    liftIO . putStr $ "open " ++ url ++ "\nverifier: "
    makeToken =<< liftIO getLine
-}