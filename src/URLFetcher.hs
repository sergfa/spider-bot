module URLFetcher
    ( fetchRequest
    , Body
    , extractDomainURLs
    , relativeToAbsoluteURLS
    , parseHostName
    , parseURIAuth
    , compareURIAuth
    ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Simple (Request, getResponseBody, getResponseStatus, httpLBS, parseRequest)
import Network.HTTP.Types (status200, statusMessage)
import Network.URI (URI, URIAuth, isRelativeReference, isURI, parseAbsoluteURI, parseRelativeReference, parseURI, uriAuthority, uriPath, uriRegName, uriScheme)

import CommonTypes (Body, PageURL)

fetchRequest :: PageURL -> IO (Either BC.ByteString Body)
fetchRequest url = do
    req <- (parseRequest . T.unpack) url
    response <- httpLBS req
    let status = getResponseStatus response
    if status == status200
        then return $ Right (getResponseBody response)
        else return $ Left $ statusMessage status

extractDomainURLs :: PageURL -> [PageURL] -> [PageURL]
extractDomainURLs domainURL = filter (\url -> compareURIAuth domainURL url || isRelativeReference (T.unpack url))

parseURIAuth :: PageURL -> Maybe URIAuth
parseURIAuth url = (parseURI . T.unpack) url >>= uriAuthority

parseHostName :: URIAuth -> Text
parseHostName = T.pack . uriRegName

delimeter :: PageURL -> Text
delimeter relativeURL
    | T.take 1 relativeURL == "/" = ""
    | otherwise = "/"

buildAbsoluteURL :: Text -> Text -> PageURL -> Text
buildAbsoluteURL host protocol url =
    if isURI $ T.unpack url
        then url
        else mconcat [protocol, "//", host, delimeter url, url]

relativeToAbsoluteURLS :: PageURL -> [PageURL] -> [PageURL]
relativeToAbsoluteURLS absoulteURL relativeURLs =
    if isNothing hostMaybe
        then []
        else transformedURLs
  where
    hostMaybe = parseHostName <$> parseURIAuth absoulteURL
    protocolMaybe = T.pack . uriScheme <$> parseAbsoluteURI (T.unpack absoulteURL)
    protocol = fromMaybe "http" protocolMaybe
    transformedURLs = map (buildAbsoluteURL (fromJust hostMaybe) protocol) relativeURLs

compareURIAuth :: PageURL -> PageURL -> Bool
compareURIAuth p1 p2
    | isRelativeReference (T.unpack p1) || isRelativeReference (T.unpack p2) = False
    | isNothing p1AuthMaybe || isNothing p2AuthMaybe = False
    | fromJust p1AuthMaybe == fromJust p2AuthMaybe = True
    | normalizeHostName (fromJust p1AuthMaybe) == normalizeHostName (fromJust p2AuthMaybe) = True
    | otherwise = False
  where
    p1AuthMaybe = parseURIAuth p1
    p2AuthMaybe = parseURIAuth p2

normalizeHostName :: URIAuth -> Text
normalizeHostName uriAuth
    | startsWithWWW = T.drop 4 hostName
    | otherwise = hostName
  where
    hostName = (T.toCaseFold . T.pack . uriRegName) uriAuth
    startsWithWWW = ((== "www.") . T.take 4) hostName
