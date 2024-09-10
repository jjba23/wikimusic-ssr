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
simpleFooter vv = H.footer ! css' ["flex", "flex-col", "justify-center", "gap-6", "align-center", "text-center", "py-6"] $ do
  (small ! css' ["italic"]) . text $ ((^. #more % #copyright0) |##| (vv ^. #language))
  a
    ! css' ["accent-color"]
    ! href "https://github.com/jjba23/wikimusic-ssr"
    $ small
    . text
    $ ((^. #more % #copyright1) |##| (vv ^. #language))
  small . text $ ((^. #more % #copyright2) |##| (vv ^. #language))

bodyWithFooter :: ViewVars -> Html -> Html
bodyWithFooter vv x = do
  body ! css' ["container", "mx-auto"] $ do
    _ <- x
    hr
    simpleFooter vv
