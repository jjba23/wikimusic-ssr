{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.HtmlUtil where

import Principium
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.SSR.View.Components.Footer
import WikiMusic.SSR.View.Components.PageTop

newtype SimplePageTitle = SimplePageTitle {value :: Text} deriving (Eq, Show)

makeFieldLabelsNoPrefix ''SimplePageTitle

mkSharedHead :: (MonadIO m) => Env -> ViewVars -> SimplePageTitle -> m Html
mkSharedHead _ _ pageTitle = do
  pure $ H.head $ do
    H.meta ! charset "utf-8"
    H.meta ! lang "en"
    H.title . text $ (pageTitle ^. #value)
    meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    H.style
      . text
      $ [trimming|
                 @import url('https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap');
                 @font-face {
                   font-family: 'Iosevka Comfy Wide';
                   src: url('https://raw.githubusercontent.com/protesilaos/iosevka-comfy/master/iosevka-comfy-wide/TTF/iosevka-comfy-wide-normalregularupright.ttf');
                 }                              
        |]
    H.script ! src "https://cdn.tailwindcss.com?plugins=forms,typography,aspect-ratio,line-clamp,container-queries" $ ""
    H.script
      . text
      $ [trimming|
      tailwind.config = {
        theme: {
          container: { center: true },
          fontFamily: {
            sans: ['Inter', 'sans-serif'],
            serif: ['Inter', 'serif'],
            mono: ['Iosevka Comfy Wide', 'monospace']
          },
        }
      }                              
    |]

simplePage :: (MonadIO m) => Env -> ViewVars -> SimplePageTitle -> Html -> m Html
simplePage env vv title' body' = do
  sharedHead <- mkSharedHead env vv title'
  pure $ H.html $ do
    sharedHead
    bodyWithFooter vv $ do
      sharedPageTop Nothing vv
      (H.div ! css' ["text-center", "my-4"]) $ do
        (H.h2 ! css' ["text-xl", "text-slate-600", "font-sans", "font-bold"]) . text $ title' ^. #value
      body'

paginationOffsetJS :: Text -> Text -> Text
paginationOffsetJS offset' newOffset =
  [trimming|(function(){
                 if(/offset/.test(window.location.toString())){
                     window.location = window.location.toString()
                       .replace("offset=$offset'", "offset=$newOffset");
                   }else{
                     window.location = window.location + "?offset=$newOffset";
                   }
                 })
                 ()|]

maybeNextPaginationButton :: ViewVars -> Limit -> Offset -> Int -> Html
maybeNextPaginationButton _ _ _ 0 = pure ()
maybeNextPaginationButton _ (Limit 0) _ _ = pure ()
maybeNextPaginationButton vv (Limit limit) (Offset offset) itemSize =
  when (itemSize == limit)
    $ H.button
    ! css (cssButton vv)
    ! onclick (textToAttrValue . minify $ paginationOffsetJS offset' newOffset)
    $ "("
    <> pageNum
    <> ") next page >"
  where
    offset' = show offset
    newOffset = show $ offset + limit
    pageNum = show $ (offset `Principium.div` limit) + 1
    minify =
      replaceText
        "\n"
        ""

maybePrevPaginationButton :: ViewVars -> Limit -> Offset -> Int -> Html
maybePrevPaginationButton _ (Limit 0) _ _ = pure ()
maybePrevPaginationButton vv (Limit limit) (Offset offset) _ =
  when (offset > 0)
    $ H.button
    ! css (cssButton vv)
    ! onclick (textToAttrValue . minify $ paginationOffsetJS offset' newOffset)
    $ "< previous page ("
    <> pageNum
    <> ")"
  where
    offset' = show offset
    newOffset = show $ offset - limit
    pageNum = show $ (offset `Principium.div` limit) - 1
    minify = replaceText "\n" ""
