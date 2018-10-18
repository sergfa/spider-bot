module CommonTypes
    ( PageURL
    , Body
    , Application(..)
    ) where

import Data.Aeson (ToJSON, (.=), object, toJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

type PageURL = Text

type Body = ByteString

data Application = Application
    { appLandingPage :: PageURL
    , appTimestamp :: Int
    , appPageURLs :: [PageURL]
    } deriving (Show)

instance ToJSON Application where
    toJSON app = object ["landingPage" .= appLandingPage app, "timestamp" .= appTimestamp app, "pages" .= appPageURLs app]
