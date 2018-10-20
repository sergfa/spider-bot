module HTMLBuilder (renderApp) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (mapM_)
import CommonTypes (Application(..) , Page (..))
import qualified Data.Text as T
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy.Char8 as LC

appToHTML :: Application -> Html
appToHTML app =
    docTypeHtml $ 
        H.head $ do
            H.title "Application Discovery"
            body $ do
                h1 (toHtml ("Application name: " `T.append` appName app))
                ul $ mapM_ (li . pageToHtml) (appPages app)


pageToHtml :: Page -> Html
pageToHtml (Page url title) = a ! href (toValue url) $ toHtml title
pageToHtml EmptyPage = p ! class_ "styled" $ em "Page Not Found"

renderApp :: Application -> LC.ByteString
renderApp  app = renderHtml (appToHTML app)