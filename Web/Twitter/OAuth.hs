module Web.Twitter.OAuth
       ( getAuthenticateURL
       , makeToken
       , Consumer(..)
       , authenticate
       , writeToken
       , readToken
       ) where

import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO, liftIO)
import Network.OAuth.Consumer
import Network.OAuth.Http.Request
import Network.OAuth.Http.CurlHttpClient
import qualified Data.ByteString.Lazy as L
import Data.Binary as B


reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"

authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++)
            . findWithDefault ("oauth_token","") . oauthParams

request :: SigMethod -> Maybe Realm -> Request -> OAuthMonadT IO Token
request = \method realm request -> signRq2 method realm request >>= oauthRequest CurlClient

data Consumer = Consumer
    { key :: String
    , secret :: String }
    deriving (Show, Eq)

getAuthenticateURL :: Consumer -> OAuthMonadT IO String
getAuthenticateURL consumer = do
    ignite $ Application (key consumer) (secret consumer) OOB 
    request HMACSHA1 Nothing reqUrl
    authUrl <$> getToken

makeToken :: String -> OAuthMonadT IO Token
makeToken answer = do
    token <- getToken
    putToken $ injectOAuthVerifier answer token
    request HMACSHA1 Nothing accUrl
    getToken

authenticate :: Consumer -> IO Token
authenticate consumer = runOAuthM (fromApplication $ Application (key consumer) (secret consumer) OOB) $ do
    url <- getAuthenticateURL consumer
    liftIO . putStr $ "open " ++ url ++ "\nverifier: "
    makeToken =<< liftIO getLine

writeToken :: Token -> FilePath -> IO ()
writeToken token path = L.writeFile path (encode token)

readToken :: FilePath -> IO Token
readToken path = fmap decode (L.readFile path)
