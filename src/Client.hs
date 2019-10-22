module Client
  ( ClientError
  , Url
  , PageTitle
  , getPageTitle
  ) where

import           Control.Exception             (Exception, SomeException,
                                                displayException, throw,
                                                toException, try)
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Either.Combinators       (mapLeft, maybeToRight)
import           Data.List                     (isPrefixOf)
import           Data.Typeable

import           Text.Regex                    (Regex, matchRegex, mkRegex)

import           Network.Connection            (TLSSettings (..))
import           Network.URI                   (isURI)

import           Network.HTTP.Conduit          (httpLbs, newManager,
                                                parseRequest, responseBody,
                                                tlsManagerSettings)

type Url = String -- TODO: refactor to newtype

type PageTitle = String -- TODO: refactor to newtype

data ClientError
  = InvalidUrl
  | InaccessibleHost
  | BadResponse
  | Redirect Url
  | NoTitle
  deriving (Show, Typeable)

instance Exception ClientError

makeRequest :: Url -> IO L.ByteString
makeRequest url = do
  putStrLn $ "--> " ++ url
  let newUrl = addHttpScheme url
  --let _ = if not $ isURI newUrl then throw InvalidUrl else () -- не работает выброс кастомного исключения!
  request <- parseRequest newUrl
  manager <- newManager tlsManagerSettings
  res <- fmap responseBody (httpLbs request manager)
  putStrLn $ "<-- " ++ url
  return res

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
addHttpScheme url =
  if checkHttpScheme url
    then url
    else "http://" ++ url

checkHttpScheme :: Url -> Bool
checkHttpScheme url = isPrefixOf "http://" url || isPrefixOf "https://" url
--mapException :: Exception -> ClientError
--mapException e = case e of
--  InvalidUrl -> InvalidUrl
--  _ -> BadResponse
