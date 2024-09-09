module WikiMusic.SSR.View.Components.Icons
  ( simpleIcon,
  )
where

import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

simpleIcon :: Text -> Text -> Html
simpleIcon x txt =
  H.span $ do
    H.span $ text x
    H.span $ text txt
