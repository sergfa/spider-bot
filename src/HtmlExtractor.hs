module HtmlExtractor
    ( extractHtml
    , PageInfo
    , ElementInfo
    , elementsInfo
    , elementName
    , attributeValueByName
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.List (group, sort)
import Data.Maybe (Maybe, catMaybes, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Text.HTML.TagSoup (Tag(..), fromAttrib, fromTagText, isTagOpen, isTagOpenName, isTagText, parseTags)

import CommonTypes (PageURL)

monitoredTags :: [Text]
monitoredTags = ["a"]

monitoredAttrs :: [Text]
monitoredAttrs = ["src", "href", "type"]

type ElementName = Text

type ElementCount = Int

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
    , elementsInfo :: [ElementInfo]
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

extractText :: Tag Text -> Maybe Text
extractText tag =
    if isTagText tag
        then Just (fromTagText tag)
        else Nothing

extractHtml :: PageURL -> LC.ByteString -> PageInfo
extractHtml url html = PageInfo url elems
  where
    tags = parseTags ((E.decodeUtf8 . L.toStrict) html)
    elems = (mapMaybe extractTag . filter isMonitoredTag) tags

attributeValueByName :: ElementInfo -> Text -> [Text]
attributeValueByName elem attrName = values
  where
    attrs = elementAttrs elem
    values = (map attributeValue . filter ((== attrName) . attributeName)) attrs
