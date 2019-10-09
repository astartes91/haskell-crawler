module Client
  ( ClientError
  , Url
  , PageTitle
  , getPageTitle
  ) where

import           Control.Exception             (SomeException, displayException,
                                                toException, try)
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Either.Combinators       (mapLeft, maybeToRight)

import           Text.Regex                    (Regex, matchRegex, mkRegex)

import           Network.Connection            (TLSSettings (..))

import           Network.HTTP.Conduit          (httpLbs, mkManagerSettings,
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
  request <- parseRequest url
  let settings = mkManagerSettings (TLSSettingsSimple False False False) Nothing
  manager <- newManager settings
  fmap responseBody (httpLbs request manager)

makeRequestE :: Url -> IO (Either ClientError L.ByteString)
makeRequestE url = fmap (\x -> mapLeft (const BadResponse) x) ioEither
  where
    ioEither = try $ makeRequest url :: IO (Either SomeException L.ByteString)

pageTitleRegex :: Regex
pageTitleRegex = mkRegex "<title[^>]*>([^<]+)</title>"

parseTitle :: L.ByteString -> Either ClientError String
parseTitle = maybeToRight NoTitle . fmap head . matchRegex pageTitleRegex . L.unpackChars

getPageTitle :: Url -> IO (Either ClientError String)
getPageTitle = fmap (>>= parseTitle) . makeRequestE
