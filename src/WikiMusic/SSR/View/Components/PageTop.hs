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
  H.div $ do
    userPrefs vv
    -- warningBanner language
    maybeTitle title'

maybeTitle :: Maybe Text -> Html
maybeTitle =
  mapM_
    ( \x ->
        H.div
          $ (h2 ! class_ "font-weight-300 font-size-xx-large")
          . text
          $ x
    )

myNav :: ViewVars -> Html
myNav vv = do
  hr
  nav ! class_ "flex flex-wrap flex-row justify-center gap-8" $ do
    a ! href "/songs" $ text ((^. #more % #songsNav) |##| (vv ^. #language))
    a ! href "/artists" $ text ((^. #more % #artistsNav) |##| (vv ^. #language))
    a ! href "/genres" $ text ((^. #more % #genresNav) |##| (vv ^. #language))
    a ! href "/login" $ text ((^. #more % #loginNav) |##| (vv ^. #language))
  hr

userPrefs :: ViewVars -> Html
userPrefs vv = do
  H.div $ do
    H.div
      $ H.form
      ! action "/user-preferences/locale"
      ! method "POST"
      ! enctype "multipart/form-data"
      $ do
        select ! onchange "this.form.submit()" ! name "locale" $ do
          option H.!? ((vv ^. #language % #value) == "en", selected "true") ! value "en" $ "🇬🇧 English"
          option H.!? ((vv ^. #language % #value) == "nl", selected "true") ! value "nl" $ "🇳🇱 Nederlands"

        noscript $ button ! type_ "submit" $ "submit"
    H.div $ H.form ! action "/user-preferences/dark-mode" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! onchange "this.form.submit()" ! type_ "checkbox" ! name "dark-mode" ! A.id "dark-mode" $ do
        option H.!? ((vv ^. #uiMode % #value) == "dark", selected "true") ! value "dark" $ simpleIcon "🌙" "dark mode"
        option H.!? ((vv ^. #uiMode % #value) == "light", selected "true") ! value "light" $ simpleIcon "☀️" "light mode"
      noscript $ button ! type_ "submit" $ "submit"
    H.div $ H.form ! action "/user-preferences/palette" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! onchange "this.form.submit()" ! type_ "checkbox" ! name "palette" ! A.id "palette" $ do
        option H.!? ((vv ^. #palette % #value) == "mauve", selected "true") ! value "mauve" $ "mauve"
        option H.!? ((vv ^. #palette % #value) == "green", selected "true") ! value "green" $ "green"
      noscript $ button ! type_ "submit" $ "submit"

topTitle :: ViewVars -> Html
topTitle vv = do
  section $ do
    a
      ! href "/songs"
      $ h1
      ! class_ "italic font-size-xxx-large font-weight-300"
      $ text ((^. #titles % #wikimusicSSR) |##| (vv ^. #language))
    em
      ! class_ "margin-top-large font-size-large font-weight-300"
      $ text ((^. #slogans % #pageTop) |##| (vv ^. #language))
