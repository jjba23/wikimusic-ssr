{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.HtmlUtil where

import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Model.Other
import WikiMusic.SSR.View.Components.Footer
import WikiMusic.SSR.View.Components.PageTop

newtype SimplePageTitle = SimplePageTitle {value :: Text} deriving (Eq, Show)

makeFieldLabelsNoPrefix ''SimplePageTitle

mkSharedHead :: (MonadIO m) => Env -> ViewVars -> SimplePageTitle -> m Html
mkSharedHead env vv pageTitle = do
  let style' = text (env ^. #mainCss)
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
    H.title . text $ (pageTitle ^. #value)
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.style . text $ modeStyle
    H.style . text $ paletteStyle
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

maybeNextPaginationButton :: Limit -> Offset -> Int -> Html
maybeNextPaginationButton _ _ 0 = pure ()
maybeNextPaginationButton (Limit 0) _ _ = pure ()
maybeNextPaginationButton (Limit limit) (Offset offset) itemSize =
  when (itemSize == limit)
    $ (H.button ! onclick (fromTextToAttributeValue func))
    . H.small
    $ "next >"
  where
    offset' = show offset
    newOffset = show $ offset + limit
    func =
      replaceText
        "\n"
        ""
        [trimming|(function(){
                 if(/^offset$/.test(window.location)){
                   window.location = window.location.replace("offset=$offset'", "offset=$newOffset");
                   }else{
                   window.location = window.location + "?offset=$newOffset";
                   }
                 })()|]

maybePrevPaginationButton :: Limit -> Offset -> Int -> Html
maybePrevPaginationButton (Limit 0) _ _ = pure ()
maybePrevPaginationButton (Limit limit) (Offset offset) _ =
  when (offset > 0)
    $ (H.button ! onclick (fromTextToAttributeValue func))
    . H.small
    $ "< prev"
  where
    offset' = show offset
    newOffset = show $ offset - limit
    func =
      replaceText
        "\n"
        ""
        [trimming|(function(){window.location = window.location.replace("offset=$offset'", "offset=$newOffset")})()|]
