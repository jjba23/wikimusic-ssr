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

detailList :: Html -> Html
detailList = dl ! class_ "margin-top-medium flex direction-column gap-small align-items-baseline"
