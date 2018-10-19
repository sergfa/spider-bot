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

import CommonTypes (Application, Application(..), PageTitle, PageURL, PageTitle, Page (..))

import HtmlExtractor (PageInfo, attributeValueByName, elementName, pageElementsInfo, extractHtml, pageTitle)
import URLFetcher (extractRelativeURLs, fetchRequest, relativeToAbsoluteURLS)

mergeDiscoveredURLs :: M.Map Text Page -> [Text] -> M.Map Text Page
mergeDiscoveredURLs discoveredURLs urls = M.unionWith (\left _-> left) discoveredURLs urlsToMap
  where
    urlsToMap = M.fromList (zip urls (repeat EmptyPage))

discovery :: Int -> M.Map PageURL Page -> [PageURL]  -> IO [Page]
discovery _ discoveredURLsDB [] = return $ M.elems discoveredURLsDB
discovery maxRecords discoveredURLsDB (url:urls) =
    if M.size discoveredURLsDB > maxRecords
        then return $ M.elems discoveredURLsDB
        else if isURI $ T.unpack url
                 then do
                     let urlDiscovered = M.lookup url discoveredURLsDB
                     if isJust urlDiscovered
                         then discovery maxRecords discoveredURLsDB urls
                         else do
                             eitherBodyOrError <- fetchRequest url
                             if isRight eitherBodyOrError
                                 then do
                                     let html = fromRight (LC.pack "") eitherBodyOrError
                                     let extractedHTML = extractHtml url html
                                     let pageData = extractPageData  extractedHTML
                                     let urls' = (combineURLs urls . relativeToAbsoluteURLS url . extractRelativeURLs . fst ) pageData
                                     let title = snd pageData     
                                     let discoveredURLsDB' = M.insert url (Page  url title) discoveredURLsDB       
                                     discovery maxRecords discoveredURLsDB' urls'
                                 else discovery maxRecords discoveredURLsDB urls
                 else discovery maxRecords discoveredURLsDB urls

extractPageData :: PageInfo -> ([PageURL], PageTitle)
extractPageData pageInfo = (urls, title)
  where
    urls = (mconcat . map (`attributeValueByName` "href") . filter ((== "a") . elementName) . pageElementsInfo) pageInfo
    title = pageTitle pageInfo
    
combineURLs :: [Text] -> [Text] -> [Text]
combineURLs xs ys = (nub . mconcat) [xs, ys]

discoveryApplication :: Int -> PageURL -> IO Application
discoveryApplication limit pageURL = do
    pages <- discovery limit M.empty [pageURL]
    timestamp <- round `fmap` getPOSIXTime
    return $ Application pageURL timestamp pages
