module Client
  ( ClientError
  , Url
  , PageTitle
  , getPageTitle
  ) where

import           Control.Exception             (SomeException, displayException,
                                                toException, try)
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Either.Combinators       (mapLeft)
import qualified Text.Parsec                   as P

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

makeRequestE :: Url -> IO (Url, Either ClientError L.ByteString)
makeRequestE url = fmap (\x -> (url, mapLeft (const BadResponse) x)) ioEither
  where
    ioEither = try $ makeRequest url :: IO (Either SomeException L.ByteString)

-- mock parser function, only takes 30 chars
parseTitle :: L.ByteString -> Either ClientError String
parseTitle = mapLeft (const NoTitle) . P.parse (P.count 30 P.anyChar) ""

getPageTitle :: Url -> IO (Url, Either ClientError String)
getPageTitle = fmap (fmap (>>= parseTitle)) . makeRequestE
