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

startDiscovery :: Int -> PageURL -> FilePath -> IO ()
startDiscovery limit page fname = do
    app <- discoveryApplication limit page
    LC.writeFile fname (encodePretty app)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n, pageURL, fname] -> startDiscovery (read n) (T.pack pageURL) fname
        _ -> TIO.putStrLn "Usage: discovery [max number of pages] [landing page URL] [ json output file name]"
