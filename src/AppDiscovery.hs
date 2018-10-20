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

import CommonTypes (Application, Application(..), PageTitle, PageURL, PageTitle, Page (..), AbsoluteURL)

import HtmlExtractor (PageInfo, attributeValueByName, elementName, pageElementsInfo, extractHtml, pageTitle)
import URLFetcher (filterDomainURLs, fetchRequest, relativeToAbsoluteURLS)

mergeDiscoveredURLs :: M.Map AbsoluteURL Page -> [AbsoluteURL] -> M.Map AbsoluteURL Page
mergeDiscoveredURLs discoveredURLs urls = M.unionWith (\left _-> left) discoveredURLs urlsToMap
  where
    urlsToMap = M.fromList (zip urls (repeat EmptyPage))

discovery :: Int -> M.Map AbsoluteURL Page -> [AbsoluteURL]  -> IO [Page]
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
                                     let urls' = (combineURLs urls . relativeToAbsoluteURLS url . filterDomainURLs url . fst ) pageData
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

discoveryApplication :: Int -> AbsoluteURL -> IO Application
discoveryApplication limit url = do
    pages <- discovery limit M.empty [url]
    timestamp <- round `fmap` getPOSIXTime
    return $ Application url timestamp pages
