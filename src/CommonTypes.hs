module CommonTypes
    ( PageURL
    , Body
    , PageTitle
    , Application(..)
    , Page (..)
    ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

type PageURL = Text

type PageTitle = Text

type Body = ByteString

emptyText:: Text
emptyText = ""

data Page
    = Page PageURL
           PageTitle
    | EmptyPage deriving Show

instance ToJSON Page where
    toJSON (Page url title) = object ["url" .= url, "title" .= title]
    toJSON EmptyPage = object ["url" .= emptyText, "title" .= emptyText]

data Application = Application
    { appLandingPage :: PageURL
    , appTimestamp :: Int
    , appPages :: [Page]
    } deriving (Show)

instance ToJSON Application where
    toJSON app = object ["landingPage" .= appLandingPage app, "timestamp" .= appTimestamp app, "pages" .= appPages app]
