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

languageMenu :: ViewVars -> Html
languageMenu vv = do
  H.form
  ! action "/user-preferences/locale"
  ! method "POST"
  ! enctype "multipart/form-data"
  $ do
    select ! css (cssSelect vv) ! onchange "this.form.submit()" ! name "locale" $ do
      mapM_ (mkLanguageOption vv) [("en", "🇬🇧 English"), ("nl", "🇳🇱 Nederlands")]
    noscript $ button ! type_ "submit" $ "submit"

mkLanguageOption :: ViewVars -> (Text, Text) -> Html
mkLanguageOption vv (locale, viewLabel) =
  ( option
      H.!? ((vv ^. #language % #value) == (fromString . unpackText $ locale), selected (fromString . unpackText $ ""))
      ! value (fromString . unpackText $ locale)
  )
    . text
    $ viewLabel

myNav :: ViewVars -> Html
myNav vv = do
  hr
  nav ! css' ["flex", "flex-row", "flex-wrap", "gap-8", "py-4", "justify-center"] $ do
    H.div ! css' ["flex", "flex-wrap", "flex-row", "justify-center", "gap-8", "items-center"] $ do
      a ! css' navLinkClass ! href "/songs" $ text ((^. #more % #songsNav) |##| (vv ^. #language))
      a ! css' navLinkClass ! href "/artists" $ text ((^. #more % #artistsNav) |##| (vv ^. #language))
      a ! css' navLinkClass ! href "/genres" $ text ((^. #more % #genresNav) |##| (vv ^. #language))
      a ! css' navLinkClass ! href "/login" $ text ((^. #more % #loginNav) |##| (vv ^. #language))
    H.div ! css' ["flex", "flex-wrap", "flex-row", "justify-center", "gap-8", "items-center"] $ do
      languageMenu vv
  hr
  where
    navLinkClass = ["text-lg", "font-bold", if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"]

userPrefs :: ViewVars -> Html
userPrefs vv = do
  H.div ! css' ["mt-6", "flex", "flex-wrap", "flex-row", "gap-6", "justify-center", "align-center"] $ do
    H.form ! action "/user-preferences/dark-mode" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! css (cssSelect vv) ! onchange "this.form.submit()" ! type_ "checkbox" ! name "dark-mode" ! A.id "dark-mode" $ do
        option H.!? ((vv ^. #uiMode % #value) == "dark", selected "true") ! value "dark" $ simpleIcon "🌙" "dark mode"
        option H.!? ((vv ^. #uiMode % #value) == "light", selected "true") ! value "light" $ simpleIcon "☀️" "light mode"
      noscript $ button ! type_ "submit" $ "submit"

    H.form ! action "/user-preferences/palette" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! css (cssSelect vv) ! onchange "this.form.submit()" ! type_ "checkbox" ! name "palette" ! A.id "palette" $ do
        mapM_
          (mkPaletteOption vv)
          ( sort
              [ "purple",
                "green",
                "amber",
                "stone",
                "white",
                "red",
                "pink",
                "orange",
                "slate",
                "gray",
                "yellow",
                "lime",
                "emerald",
                "teal",
                "cyan",
                "sky",
                "blue",
                "indigo",
                "violet",
                "rose"
              ]
          )
      noscript $ button ! type_ "submit" $ "submit"

mkPaletteOption :: ViewVars -> Text -> Html
mkPaletteOption vv choice = do
  (option H.!? ((vv ^. #palette % #value) == choice, selected "true") ! value (fromString . unpackText $ choice)) . text $ choice

topTitle :: ViewVars -> Html
topTitle vv = do
  section
    ! css'
      [ "flex",
        "flex-wrap",
        "flex-row",
        "justify-center",
        "align-center",
        "gap-8",
        "px-3",
        "py-3"
      ]
    $ do
      a
        ! href "/songs"
        $ h1
        ! css'
          [ "text-xl",
            "font-bold",
            "italic",
            if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"
          ]
        $ text ((^. #titles % #wikimusicSSR) |##| (vv ^. #language))
      em
        ! css'
          [ "text-xl",
            "font-light",
            "italic",
            if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"
          ]
        $ text ((^. #slogans % #pageTop) |##| (vv ^. #language))
