{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.DetailList where

import Principium
import Text.Blaze.Html5 as H

detailListEntry' :: Bool -> Text -> Html -> Html
detailListEntry' isMono key val =
  H.div
    ! css'
      [ "flex",
        "flex-row",
        "gap-4",
        "w-fit",
        "font-sans"
      ]
    $ do
      (dt ! css' ["text-gray-600"]) . text $ key
      dd ! css' ["text-gray-500", if isMono then "font-mono" else "font-sans"] $ val

detailListEntry :: Text -> Html -> Html
detailListEntry = detailListEntry' False

monoDetailListEntry :: Text -> Html -> Html
monoDetailListEntry = detailListEntry' True

detailList :: Html -> Html
detailList = dl ! css' ["flex", "flex-wrap", "flex-row", "gap-2", "justify-center"]
