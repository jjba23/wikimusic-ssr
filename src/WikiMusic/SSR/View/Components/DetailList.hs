{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.Components.DetailList
  ( detailList,
    detailListEntry,
  )
where

import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

detailListEntry :: Text -> Html -> Html
detailListEntry key val =
  H.div ! class_ "flex direction-row gap-small align-items-baseline" $ do
    dt . strong . text $ key
    dd val

detailList :: Html -> Html
detailList = dl ! class_ "margin-top-medium flex direction-column gap-small align-items-baseline"
