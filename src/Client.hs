module Client
  ( ClientError
  , Url
  , PageTitle
  , getPageTitle
  ) where

import           Control.Exception             (SomeException, displayException,
                                                toException, try)
import qualified Data.ByteString.Lazy.Internal as L
import Data.List
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

makeRequest :: Url -> IO L.ByteString
makeRequest url = do
  let newUrl = addHttpScheme url
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

addHttpScheme :: Url -> Url
addHttpScheme url = if checkHttpScheme url then url else "http://" ++ url

checkHttpScheme :: Url -> Bool
checkHttpScheme url = isPrefixOf "http://" url || isPrefixOf "https://" url