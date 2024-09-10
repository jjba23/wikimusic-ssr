{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.DetailList
  ( detailList,
    detailListEntry,
  )
where

import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

detailListEntry :: Text -> Html -> Html
detailListEntry key val =
  H.div $ do
    dt . strong . text $ key
    dd val

detailList :: Maybe Class -> Html -> Html
detailList maybeGap = dl ! class_ (["flex", "flex-wrap", "flex-row", gapClass, "justify-center"] @@)
  where
    gapClass = maybe "gap-8" (^. #value) maybeGap
