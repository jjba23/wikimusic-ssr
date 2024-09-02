{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.Components.Icons
  ( simpleIcon,
  )
where

import Data.Text qualified as T
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

simpleIcon :: Text -> Text -> Html
simpleIcon x txt =
  H.span $ do
    H.span ! class_ "margin-medium" $ (fromString . T.unpack $ x)
    H.span (fromString . T.unpack $ txt)
