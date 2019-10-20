module Client
  ( ClientError
  , Url
  , PageTitle
  , getPageTitle
  ) where

import           Control.Exception             (SomeException, displayException,
                                                toException, try)
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Text as T
import           Data.Either.Combinators       (mapLeft, maybeToRight)

import           Text.Regex                    (Regex, matchRegex, mkRegex)

import           Network.Connection            (TLSSettings (..))

import           Network.HTTP.Conduit          (httpLbs, tlsManagerSettings,
                                                newManager, parseRequest,
                                                responseBody)

type Url = String -- TODO: refactor to newtype

type PageTitle = String -- TODO: refactor to newtype

data ClientError
  = InvalidUrl
  | InaccessibleHost
  | BadResponse
  | Redirect Url
  | NoTitle
  deriving (Show)

makeRequest :: String -> IO L.ByteString
makeRequest url = do
  let textUrl = T.pack url
--  let newUrl = 
--    case T.isPrefixOf(T.pack "http://" textUrl) || T.isPrefixOf(T.pack "https://" textUrl) of
--    True -> url
--    _ -> "http://" ++ url 
--    if(T.isPrefixOf(T.pack "http://" textUrl) || T.isPrefixOf(T.pack "https://" textUrl))
--      then url  
--    else "http://" ++ url 
  let newUrl = "http://" ++ url 
  request <- parseRequest newUrl
  manager <- newManager tlsManagerSettings
  fmap responseBody (httpLbs request manager)

makeRequestE :: Url -> IO (Url, Either ClientError L.ByteString)
makeRequestE url = fmap (\x -> (url, mapLeft (const BadResponse) x)) ioEither
  where
    ioEither = try $ makeRequest url :: IO (Either SomeException L.ByteString)

pageTitleRegex :: Regex
pageTitleRegex = mkRegex "<title[^>]*>([^<]+)</title>"

parseTitle :: L.ByteString -> Either ClientError String
parseTitle = maybeToRight NoTitle . fmap head . matchRegex pageTitleRegex . L.unpackChars

getPageTitle :: Url -> IO (Url, Either ClientError String)
getPageTitle = fmap (fmap (>>= parseTitle)) . makeRequestE
