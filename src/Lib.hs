{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Lib
  ( startServer
  ) where

import           Control.Exception             (SomeException, displayException,
                                                toException, try)
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Either.Combinators       (mapBoth, mapLeft)
import           Data.List                     (intercalate)
import           Data.Maybe                    (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

-- parsing
import qualified Data.Aeson                    as A
import GHC.Generics
import Data.Aeson
import qualified Text.Parsec                   as P

-- http server
import           Network.HTTP.Types            (ResponseHeaders, hContentType,
                                                status200)
import           Network.Wai                   (Application, Request, Response,
                                                requestBody, requestHeaders,
                                                requestMethod, responseLBS)
import           Network.Wai.Handler.Warp      (run)

-- http client related
import           Control.Lens                  ((^.))
import           Network.Wreq                  (get, responseBody)
import qualified Network.Wreq                  as Wreq

type Url = String -- TODO: refactor to newtype

type PageTitle = String -- TODO: refactor to newtype

data CrawlerError
  = InvalidUrl
  | InaccessibleHost
  | BadResponse
  | Redirect Url
  | NoTitle
  deriving (Show)

-- configuration
portNumber :: Int
portNumber = 8080

startServer :: IO ()
startServer = do
  putStrLn $ "Server started at http://localhost:" ++ show portNumber
  run portNumber app

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app req respond = do
  urls <- parseRequest req :: IO [String]
  pageBodies <- mapM makeRequest urls :: IO [(Url, Either CrawlerError L.ByteString)]
  let pageTitles = fmap (fmap (>>= parseTitle)) pageBodies :: [(Url, Either CrawlerError String)]
  let jsonResponse = serializeResponses pageTitles :: L.ByteString
  respond $ buildResponse jsonResponse

-- IO intentionally fails if parsing failed, which causes a response with error code
parseRequest :: Request -> IO [String]
parseRequest req = do
  let method = requestMethod req
  jsonRequestBody <- requestBody req -- TODO: переделать на новый API: getRequestBodyChunk
  maybe (fail "damn!") return (A.decodeStrict jsonRequestBody :: Maybe [String]) -- TODO: explicitly respond with 4xx

makeRequest :: String -> IO (Url, Either CrawlerError L.ByteString)
makeRequest url = fmap (\x -> (url, mapBoth describeError extractBody x)) ioEither
  where
    describeError = const BadResponse
    extractBody = (^. responseBody)
    ioEither = try . get $ url :: IO (Either SomeException (Wreq.Response L.ByteString))

-- mock parser function, only takes 30 chars
parseTitle :: L.ByteString -> Either CrawlerError String
parseTitle = mapLeft (const NoTitle) . P.parse (P.count 30 P.anyChar) ""

serializeResponses :: [(Url, Either CrawlerError PageTitle)] -> L.ByteString
serializeResponses xs = encode $ map createCrawlerResult xs

createCrawlerResult :: (Url, Either CrawlerError PageTitle) -> HashMap String String
createCrawlerResult (url, Left e) = Map.fromList [("url", url),("error", show e)]
createCrawlerResult (url, Right title) = Map.fromList [("url", url),("title", title)]

buildResponse :: L.ByteString -> Response
buildResponse = responseLBS status200 [(hContentType, B.packChars "application/json")]
