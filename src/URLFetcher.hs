module URLFetcher
    ( fetchRequest
    , Body
    , extractRelativeURLs
    , relativeToAbsoluteURLS
    , parseHostName
    , parseURIAuth
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatus, httpLBS, parseRequest)
import Network.HTTP.Types (status200, statusMessage)
import Network.URI (URI, URIAuth, parseAbsoluteURI, parseRelativeReference, parseURI, uriAuthority, uriPath, uriRegName, uriScheme)

import CommonTypes (Body)

fetchRequest :: Text -> IO (Either BC.ByteString Body)
fetchRequest url = do
    req <- (parseRequest . T.unpack) url
    response <- httpLBS req
    let status = getResponseStatus response
    if status == status200
        then return $ Right (getResponseBody response)
        else return $ Left $ statusMessage status

extractRelativeURLs :: [Text] -> [Text]
extractRelativeURLs = map (T.pack . show) . mapMaybe (parseRelativeReference . T.unpack)

parseURIAuth :: Text -> Maybe URIAuth
parseURIAuth url = (parseAbsoluteURI . T.unpack) url >>= uriAuthority

parseHostName :: URIAuth -> Text
parseHostName = T.pack . uriRegName

delimeter :: Text -> Text
delimeter relativeURL
    | T.take 1 relativeURL == "/" = ""
    | otherwise = "/"

buildAbsoluteURL :: Text -> Text -> Text -> Text
buildAbsoluteURL host protocol relativeURL = mconcat [protocol, "//", host, delimeter relativeURL, relativeURL]

relativeToAbsoluteURLS :: Text -> [Text] -> [Text]
relativeToAbsoluteURLS absoulteURL relativeURLs =
    if isNothing hostMaybe
        then []
        else transformedURLs
  where
    hostMaybe = parseHostName <$> parseURIAuth absoulteURL
    protocolMaybe = T.pack . uriScheme <$> parseAbsoluteURI (T.unpack absoulteURL)
    protocol = fromMaybe "http" protocolMaybe
    transformedURLs = map (buildAbsoluteURL (fromJust hostMaybe) protocol) relativeURLs
