module CommonTypes
    ( PageURL
    , AbsoluteURL
    , RelativeURL
    , Body
    , PageTitle
    , Application(..)
    , Page(..)
    ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T

type PageURL = Text

type AbsoluteURL = Text

type RelativeURL = Text

type PageTitle = Text

type Body = ByteString

emptyText :: Text
emptyText = ""

data Page
    = Page PageURL
           PageTitle
    | EmptyPage
    deriving (Show, Eq, Ord)

instance ToJSON Page where
    toJSON (Page url title) = object ["url" .= url, "title" .= title]
    toJSON EmptyPage = object ["url" .= emptyText, "title" .= emptyText]

data Application = Application
    { appLandingPage :: PageURL
    , appTimestamp :: Int
    , appPages :: [Page]
    , appName :: Text
    } deriving (Show, Eq)

instance Ord Application where
    compare (Application l1 t1 p1 n1) (Application l2 t2 p2 n2)
        | nCompare /= EQ = nCompare
        | lpCompare /= EQ = lpCompare
        | tCompare /= EQ = tCompare
        | otherwise = compare (sort p1) (sort p2)
      where
        lpCompare = compare l1 l2
        tCompare = compare t1 t2
        nCompare = compare n1 n2

instance ToJSON Application where
    toJSON app = object ["landingPage" .= appLandingPage app, "timestamp" .= appTimestamp app, "pages" .= appPages app, "name" .= appName app]
