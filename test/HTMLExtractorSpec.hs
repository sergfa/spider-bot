module HTMLExtractorSpec
    ( spec
    ) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Text (Text)
import Test.Hspec

import HtmlExtractor

pageInfo :: PageInfo
pageInfo = extractHtml "https://example.com" "<title>Haskell Test</title><div>Hello Page1</div><a href=\"/test.html\" ></a>"

spec :: Spec
spec =
    describe "HtmlExtractor" $ 
        describe "extractHtml" $ do
            it "can parse title" $ pageTitle pageInfo `shouldBe` "Haskell Test\n"
            it "can parse pageUrl" $ pageURL pageInfo `shouldBe` "https://example.com"
            it "can parse ancor tag" $ (length . filter ((== "a") . elementName) . pageElementsInfo) pageInfo `shouldBe` 1
            it "can parse href attribute" $ ((`attributeValueByName` "href") . head . filter ((== "a") . elementName) . pageElementsInfo) pageInfo `shouldBe` ["/test.html"]
