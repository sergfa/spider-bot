module Main where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Environment

import AppDiscovery (discoveryApplication)
import CommonTypes (Application(..), PageURL)
import HTMLBuilder (renderApp)

startDiscovery :: Int -> PageURL -> Text -> IO ()
startDiscovery limit page appName = do
    app <- discoveryApplication limit page appName
    LC.writeFile (T.unpack(appName `T.append` ".json")) (encodePretty app)
    LC.writeFile (T.unpack(appName `T.append` ".html")) (renderApp app)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n, pageURL, appName] -> startDiscovery (read n) (T.pack pageURL) (T.pack appName)
        _ -> TIO.putStrLn "Usage: discovery [max number of pages] [landing page URL] [application name]"
