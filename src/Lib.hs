-- OverloadedStrings implicitly converts [Char] to ByteString or Text
-- QuasiQuotes is used only for multiline string literals between [r| |]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
  ( startServer, makeRequest
  ) where

-- http server
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseLBS)
import           Network.Wai.Handler.Warp (run)

-- only for multiline string literals
import           Text.RawString.QQ

-- http client related
import Network.Wreq (get, responseBody)
import qualified Data.ByteString.Lazy.Internal as B
import Control.Exception (try, SomeException)
import Control.Lens ((^.))

-- configuration
portNumber :: Int
portNumber = 8080

-- this request handler responds with hardcoded json on EVERY request
app :: Application -- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app _ respond = do
  putStrLn "There was a request. Fetching Yandex now!"
  eitherExceptionString <- makeRequest "https://ya.ru"
  putStrLn $ take 1000 $ "Fetching result: " ++ show eitherExceptionString
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json")]
      [r|
        [
           {
             "url": "https://ya.ru",
             "title": "Яндекс"
           },
           {
             "url": "www.dictionary.com/browse/http",
             "title": "Http | Define Http at Dictionary.com"
           },
           {
             "url": "https://api.github.com/",
             "error": "Page has no title"
           },
           {
             "url": "http://ya.ru",
             "error": "Redirect: https://ya.ru/"
           },
           {
             "url": "wtf://yandex.ru",
             "error": "Bad url"
           },
           {
             "url": "https://github.com/ladsgfadg",
             "error": "Status code: 404"
           },
           {
             "url": "adsfgasdfg",
             "error": "Inaccessible host"
           },
           {
             "url": "",
             "error": "Inaccessible host"
           },
           {
             "url": "https://ya.ru",
             "title": "Яндекс"
           }
         ]
         |]

startServer :: IO ()
startServer = do
  putStrLn $ "Server started at http://localhost:" ++ show portNumber
  run portNumber app

makeRequest :: String -> IO (Either SomeException B.ByteString)
makeRequest = fmap (fmap (^. responseBody)) . try . get
