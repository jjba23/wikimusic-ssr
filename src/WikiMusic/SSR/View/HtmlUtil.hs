{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.HtmlUtil where

import Data.Text qualified as T
import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.SSR.View.Components.Footer
import WikiMusic.SSR.View.Components.PageTop

newtype SimplePageTitle = SimplePageTitle {value :: Text} deriving (Eq, Show)

makeFieldLabelsNoPrefix ''SimplePageTitle

mkSharedHead :: (MonadIO m) => Env -> ViewVars -> SimplePageTitle -> m Html
mkSharedHead env vv pageTitle = do
  let style' = fromString . T.unpack $ (env ^. #mainCss)
  let modeStyle =
        if (vv ^. #uiMode % #value) == "dark"
          then env ^. #darkCss
          else env ^. #lightCss
  let paletteStyle = case vv ^. #palette % #value of
        "green" -> env ^. #palettes % #green
        _ -> env ^. #palettes % #mauve

  pure $ H.head $ do
    H.meta ! charset "utf-8"
    H.meta ! lang "en"
    H.title . fromString . T.unpack $ (pageTitle ^. #value)
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.style $ fromString . T.unpack $ modeStyle
    H.style $ fromString . T.unpack $ paletteStyle
    H.style style'

simplePage :: (MonadIO m) => Env -> ViewVars -> SimplePageTitle -> Html -> m Html
simplePage env vv title' body' = do
  sharedHead <- mkSharedHead env vv title'
  pure $ H.html $ do
    sharedHead
    bodyWithFooter vv $ do
      sharedPageTop Nothing vv
      (H.h2 ! A.class_ "margin-top-small page-title") . text $ title' ^. #value
      body'
