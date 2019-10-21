module Server
  ( startServer
  ) where
    
import qualified Data.ByteString               as B
import           Data.ByteString.Internal      (packChars)
import qualified Data.ByteString.Lazy.Internal as L

-- parsing, serialization
import qualified Data.Aeson                    as A
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as Map

-- http server
import           Network.HTTP.Types            (ResponseHeaders, hContentType,
                                                status200)
import           Network.Wai                   (Application, Request, Response,
                                                getRequestBodyChunk, requestHeaders,
                                                requestMethod, responseLBS)
import           Network.Wai.Handler.Warp      (run)

import           Client

import Control.Concurrent.Async (mapConcurrently)

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
  urls <- parseRequest req :: IO [Url]
  pageTitles <- mapConcurrently getPageTitle urls :: IO [(Url, Either ClientError String)]
  let jsonResponse = serializeResponses pageTitles :: L.ByteString
  respond $ buildResponse jsonResponse

-- IO intentionally fails if parsing failed, which causes a response with error code
parseRequest :: Request -> IO [Url]
parseRequest req = do
  jsonRequestBody <- getRequestBody req
  maybe (fail "damn!") return (A.decodeStrict jsonRequestBody :: Maybe [String]) -- TODO: explicitly respond with 4xx

getRequestBody :: Request -> IO B.ByteString
getRequestBody request = B.concat <$> getChunks
  where
    getChunks = getRequestBodyChunk request >>= \chunk ->
        if chunk == B.empty
        then pure []
        else (chunk:) <$> getChunks

serializeResponses :: [(Url, Either ClientError PageTitle)] -> L.ByteString
serializeResponses xs = A.encode $ map createCrawlerResult xs

createCrawlerResult :: (Url, Either ClientError PageTitle) -> HashMap String String
createCrawlerResult (url, Left e) = Map.fromList [("url", url), ("error", show e)]
createCrawlerResult (url, Right title) = Map.fromList [("url", url), ("title", title)]

buildResponse :: L.ByteString -> Response
buildResponse = responseLBS status200 [(hContentType, packChars "application/json")]
