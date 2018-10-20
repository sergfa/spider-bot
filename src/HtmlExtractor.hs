module HtmlExtractor
    ( extractHtml
    , PageInfo
    , ElementInfo
    , elementName
    , attributeValueByName
    , pageTitle
    , pageURL
    , pageElementsInfo
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (group, sort)
import Data.Maybe (Maybe, catMaybes, mapMaybe)
import Data.Either (fromRight)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Text.HTML.TagSoup (Tag(..), (~==), fromAttrib, fromTagText, isTagOpen, isTagOpenName, isTagText, parseTags, sections)

import CommonTypes (PageURL, AbsoluteURL)

monitoredTags :: [Text]
monitoredTags = ["a"]

monitoredAttrs :: [Text]
monitoredAttrs = ["src", "href", "type"]

type ElementName = Text


data ElementAttribute = ElementAttribute
    { attributeName :: Text
    , attributeValue :: Text
    } deriving (Show)

data ElementInfo = ElementInfo
    { elementName :: ElementName
    , elementAttrs :: [ElementAttribute]
    } deriving (Show)

data PageInfo = PageInfo
    { pageURL :: PageURL
    , pageElementsInfo :: [ElementInfo]
    , pageTitle :: Text
    } deriving (Show)

isMonitoredTag :: Tag Text -> Bool
isMonitoredTag tag = any (`isTagOpenName` tag) monitoredTags

tagName :: Tag Text -> Maybe Text
tagName (TagOpen n _) = Just n
tagName _ = Nothing

extractTag :: Tag Text -> Maybe ElementInfo
extractTag tag = ElementInfo <$> tagName tag <*> Just attr
  where
    attr = extractAttributes tag

extractAttributes :: Tag Text -> [ElementAttribute]
extractAttributes = map createAttribute . filter (not . T.null . snd) . parseAttributes

parseAttributes :: Tag Text -> [(Text, Text)]
parseAttributes tag = map (\attr -> (attr, attr `fromAttrib` tag)) monitoredAttrs

createAttribute :: (Text, Text) -> ElementAttribute
createAttribute (name, value) = ElementAttribute name value

extractText :: Tag Text -> Text
extractText tag =
    if isTagText tag
        then fromTagText tag
        else ""

extractHtml :: AbsoluteURL -> LC.ByteString -> PageInfo
extractHtml url html = PageInfo url elems title
  where
    tags = parseTags ((fromRight "" . E.decodeUtf8' . L.toStrict) html)
    elems = (mapMaybe extractTag . filter isMonitoredTag) tags
    titleTag :: String
    titleTag = "<title>"
    titles = map f $ sections (~== titleTag) tags
    title = T.unlines titles
    f = fromTagText . head . filter isTagText

attributeValueByName :: ElementInfo -> Text -> [Text]
attributeValueByName elem attrName = values
  where
    attrs = elementAttrs elem
    values = (map attributeValue . filter ((== attrName) . attributeName)) attrs
