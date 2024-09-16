{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.DetailList where

import Principium
import Text.Blaze.Html5 as H

detailListEntry' :: ViewVars -> Bool -> Text -> Html -> Html
detailListEntry' vv isMono key val =
  H.div
    ! css'
      [ "flex",
        "flex-row",
        "gap-4",
        "w-fit",
        "font-sans"
      ]
    $ do
      (dt ! css' ["text-sm", if vv ^. #uiMode % #value == "dark" then "text-gray-300" else "text-gray-700"]) . text $ key
      dd ! css' ["text-sm", "break-all", if vv ^. #uiMode % #value == "dark" then "text-gray-300" else "text-gray-700", if isMono then "font-mono" else "font-sans"] $ val

detailListEntry :: ViewVars -> Text -> Html -> Html
detailListEntry vv = detailListEntry' vv False

monoDetailListEntry :: ViewVars -> Text -> Html -> Html
monoDetailListEntry vv = detailListEntry' vv True

detailList :: Html -> Html
detailList = dl ! css' ["flex", "flex-col", "gap-2", "justify-start"]
