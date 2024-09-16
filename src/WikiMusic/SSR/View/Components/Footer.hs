{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.Footer
  ( simpleFooter,
    bodyWithFooter,
  )
where

import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

simpleFooter :: ViewVars -> Html
simpleFooter vv = H.footer ! css' ["flex", "flex-col", "justify-center", "gap-4", "align-center", "text-center", "py-6"] $ do
  (small ! css' ["italic", if vv ^. #uiMode % #value == "dark" then "text-gray-400" else "text-gray-700"]) . text $ ((^. #more % #copyright0) |##| (vv ^. #language))
  a
    ! css (cssLink vv)
    ! href "https://github.com/jjba23/wikimusic-ssr"
    $ small
    . text
    $ ((^. #more % #copyright1) |##| (vv ^. #language))
  (small ! css' [if vv ^. #uiMode % #value == "dark" then "text-gray-400" else "text-gray-700"]) . text $ ((^. #more % #copyright2) |##| (vv ^. #language))

bodyWithFooter :: ViewVars -> Html -> Html
bodyWithFooter vv x = do
  body ! css' ["bg-gradient-to-br", "from-" <> vv ^. #palette % #value <> "-200", "to-" <> vv ^. #palette % #value <> "-300"] $ do
    H.div ! css' [if vv ^. #uiMode % #value == "dark" then "bg-black/80" else "bg-white/80", "h-auto"] $ do
      _ <- x
      hr
      simpleFooter vv
