-- OverloadedStrings implicitly [Char] to ByteString or Text
-- QuasiQuotes is used only for multiline string literals between [r| |]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
  ( startServer
  ) where

-- http server
import           Network.HTTP.Types       (status200)
import           Network.Wai              (Application, responseLBS)
import           Network.Wai.Handler.Warp (run)

-- only for multiline string literals
import           Text.RawString.QQ

-- configuration
portNumber :: Int
portNumber = 8080

-- this request handler responds with hardcoded json on EVERY request
app :: Application -- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app _ respond = do
  putStrLn "There was a request"
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
