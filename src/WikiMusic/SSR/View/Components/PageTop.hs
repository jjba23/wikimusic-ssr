{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.PageTop where

import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.SSR.View.Components.Icons

sharedPageTop :: Maybe Text -> ViewVars -> Html
sharedPageTop title' vv = do
  topTitle vv
  myNav vv

  H.div ! css' ["flex", "flex-wrap", "flex-row", "gap-6", "justify-center"] $ do
    userPrefs vv
    -- warningBanner language
    maybeTitle title'

maybeTitle :: Maybe Text -> Html
maybeTitle =
  mapM_
    ( (h2 ! css' ["text-2xl", "text-grey-600"])
        . text
    )

myNav :: ViewVars -> Html
myNav vv = do
  hr
  nav ! css' ["flex", "flex-wrap", "flex-row", "justify-center", "gap-8", "py-4"] $ do
    a ! class_ "text-lg font-bold" ! href "/songs" $ text ((^. #more % #songsNav) |##| (vv ^. #language))
    a ! class_ "text-lg font-bold" ! href "/artists" $ text ((^. #more % #artistsNav) |##| (vv ^. #language))
    a ! class_ "text-lg font-bold" ! href "/genres" $ text ((^. #more % #genresNav) |##| (vv ^. #language))
    a ! class_ "text-lg font-bold" ! href "/login" $ text ((^. #more % #loginNav) |##| (vv ^. #language))
  hr

userPrefs :: ViewVars -> Html
userPrefs vv = do
  H.div ! css' ["mt-6", "flex", "flex-wrap", "flex-row", "gap-6", "justify-center", "align-center"] $ do
    H.form
      ! action "/user-preferences/locale"
      ! method "POST"
      ! enctype "multipart/form-data"
      $ do
        select ! css cssSelect ! onchange "this.form.submit()" ! name "locale" $ do
          option H.!? ((vv ^. #language % #value) == "en", selected "true") ! value "en" $ "🇬🇧 English"
          option H.!? ((vv ^. #language % #value) == "nl", selected "true") ! value "nl" $ "🇳🇱 Nederlands"
        noscript $ button ! type_ "submit" $ "submit"

    H.form ! action "/user-preferences/dark-mode" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! css cssSelect ! onchange "this.form.submit()" ! type_ "checkbox" ! name "dark-mode" ! A.id "dark-mode" $ do
        option H.!? ((vv ^. #uiMode % #value) == "dark", selected "true") ! value "dark" $ simpleIcon "🌙" "dark mode"
        option H.!? ((vv ^. #uiMode % #value) == "light", selected "true") ! value "light" $ simpleIcon "☀️" "light mode"
      noscript $ button ! type_ "submit" $ "submit"

    H.form ! action "/user-preferences/palette" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! css cssSelect ! onchange "this.form.submit()" ! type_ "checkbox" ! name "palette" ! A.id "palette" $ do
        option H.!? ((vv ^. #palette % #value) == "mauve", selected "true") ! value "mauve" $ "mauve"
        option H.!? ((vv ^. #palette % #value) == "green", selected "true") ! value "green" $ "green"
      noscript $ button ! type_ "submit" $ "submit"

topTitle :: ViewVars -> Html
topTitle vv = do
  section ! css' ["flex", "flex-wrap", "flex-row", "justify-center", "align-center", "gap-8", "px-3", "py-3"] $ do
    a
      ! href "/songs"
      $ h1
      ! css' ["text-xl", "font-bold", "italic"]
      $ text ((^. #titles % #wikimusicSSR) |##| (vv ^. #language))
    em
      ! css' ["text-xl", "font-light", "italic"]
      $ text ((^. #slogans % #pageTop) |##| (vv ^. #language))
