{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.Components.PageTop
  ( sharedPageTop,
  )
where

import Data.Text qualified as T
import Optics
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.View.Components.Icons

sharedPageTop :: Maybe Text -> ViewVars -> Html
sharedPageTop title' vv = do
  topTitle vv
  myNav vv
  H.div ! class_ "flex direction-column" $ do
    userPrefs vv
    -- warningBanner language
    maybeTitle title'

maybeTitle :: Maybe Text -> Html
maybeTitle = mapM_ (\x -> H.div ! class_ "text-align-center" $ (h2 ! class_ "font-weight-300 font-size-xx-large") . fromString . T.unpack $ x)

myNav :: ViewVars -> Html
myNav vv = do
  hr
  nav $ ol $ do
    li $ a ! class_ "nav-link scale-on-hover" ! href "/songs" $ text ((^. #more % #songsNav) |##| (vv ^. #language))
    li $ a ! class_ "nav-link scale-on-hover" ! href "/artists" $ text ((^. #more % #artistsNav) |##| (vv ^. #language))
    li $ a ! class_ "nav-link scale-on-hover" ! href "/genres" $ text ((^. #more % #genresNav) |##| (vv ^. #language))
    li $ a ! class_ "nav-link scale-on-hover" ! href "/login" $ text ((^. #more % #loginNav) |##| (vv ^. #language))
  hr

userPrefs :: ViewVars -> Html
userPrefs vv = do
  H.div ! class_ "flex direction-row justify-content-center gap-small" $ do
    H.div
      $ H.form
      ! action "/user-preferences/locale"
      ! method "POST"
      ! enctype "multipart/form-data"
      $ do
        select ! onchange "this.form.submit()" ! name "locale" $ do
          option !? ((vv ^. #language % #value) == "en", selected "true") ! value "en" $ "🇬🇧 English"
          option !? ((vv ^. #language % #value) == "nl", selected "true") ! value "nl" $ "🇳🇱 Nederlands"

        noscript $ button ! type_ "submit" $ "submit"
    H.div $ H.form ! action "/user-preferences/dark-mode" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! onchange "this.form.submit()" ! type_ "checkbox" ! name "dark-mode" ! A.id "dark-mode" $ do
        option !? ((vv ^. #uiMode % #value) == "dark", selected "true") ! value "dark" $ simpleIcon "🌙" "dark mode"
        option !? ((vv ^. #uiMode % #value) == "light", selected "true") ! value "light" $ simpleIcon "☀️" "light mode"
      noscript $ button ! type_ "submit" $ "submit"
    H.div $ H.form ! action "/user-preferences/palette" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! onchange "this.form.submit()" ! type_ "checkbox" ! name "palette" ! A.id "palette" $ do
        option !? ((vv ^. #palette % #value) == "mauve", selected "true") ! value "mauve" $ "mauve"
        option !? ((vv ^. #palette % #value) == "green", selected "true") ! value "green" $ "green"
      noscript $ button ! type_ "submit" $ "submit"

topTitle :: ViewVars -> Html
topTitle vv = do
  section ! class_ "flex direction-column flex items-center" $ do
    a
      ! href "/songs"
      $ h1
      ! class_ "italic font-size-xxx-large font-weight-300"
      $ text ((^. #titles % #wikimusicSSR) |##| (vv ^. #language))
    em
      ! class_ "margin-top-large font-size-large font-weight-300"
      $ text ((^. #slogans % #pageTop) |##| (vv ^. #language))