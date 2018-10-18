module AppDiscovery
    ( discoveryApplication
    ) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Either
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Network.URI (isURI)

import CommonTypes (Application, Application(..), PageURL)

import HtmlExtractor (PageInfo, attributeValueByName, elementName, elementsInfo, extractHtml)
import URLFetcher (extractRelativeURLs, fetchRequest, relativeToAbsoluteURLS)

mergeDiscoveredURLs :: M.Map Text Bool -> [Text] -> M.Map Text Bool
mergeDiscoveredURLs discoveredURLs urls = M.unionWith (||) discoveredURLs urlsToMap
  where
    urlsToMap = M.fromList (zip urls (repeat False))

discovery :: Int -> M.Map PageURL Bool -> [PageURL] -> IO [PageURL]
discovery _ discoveredURLsDB [] = return $ M.keys discoveredURLsDB
discovery maxRecords discoveredURLsDB (url:urls) =
    if M.size discoveredURLsDB > maxRecords
        then return $ M.keys discoveredURLsDB
        else if isURI $ T.unpack url
                 then do
                     let urlDiscovered = M.lookup url discoveredURLsDB
                     if isJust urlDiscovered && fromJust urlDiscovered
                         then discovery maxRecords discoveredURLsDB urls
                         else do
                             eitherBodyOrError <- fetchRequest url
                             let discoveredURLsDB' = M.insert url True discoveredURLsDB
                             if isRight eitherBodyOrError
                                 then do
                                     let urls' =
                                             ((combineURLs urls . relativeToAbsoluteURLS url . extractRelativeURLs . extractURLs . extractHtml url)
                                                  (fromRight (LC.pack "") eitherBodyOrError))
                                     let mergedURLsDB = mergeDiscoveredURLs discoveredURLsDB' urls'
                                     discovery maxRecords mergedURLsDB urls'
                                 else discovery maxRecords discoveredURLsDB' urls
                 else discovery maxRecords discoveredURLsDB urls

extractURLs :: PageInfo -> [Text]
extractURLs = mconcat . map (`attributeValueByName` "href") . filter ((== "a") . elementName) . elementsInfo

combineURLs :: [Text] -> [Text] -> [Text]
combineURLs xs ys = (nub . mconcat) [xs, ys]

discoveryApplication :: Int -> PageURL -> IO Application
discoveryApplication limit pageURL = do
    pages <- discovery limit M.empty [pageURL]
    timestamp <- round `fmap` getPOSIXTime
    return $ Application pageURL timestamp pages
