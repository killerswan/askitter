module Web.Twitter.OAuth
       ( getAuthenticateURL
       , makeToken
       , Consumer(..)
       , authenticate
       , unwrap
       , writeToken
       , readToken
       ) where

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO, liftIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request (parseURL, findWithDefault)
import Network.OAuth.Http.HttpClient (HttpClient, unCurlM)
import qualified Data.ByteString.Lazy as L
import Data.Binary as B


reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"

authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
            . findWithDefault ("oauth_token","") . oauthParams

unwrap = unCurlM . runOAuth

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

authenticate :: Consumer -> IO Token
authenticate consumer = unwrap $ do
    ignite $ Application (key consumer) (secret consumer) OOB 
    url <- getAuthenticateURL consumer
    liftIO . putStr $ "open " ++ url ++ "\nverifier: "
    makeToken =<< liftIO getLine

writeToken :: Token -> FilePath -> IO ()
writeToken token path = L.writeFile path (encode token)

readToken :: FilePath -> IO Token
readToken path = fmap decode (L.readFile path)