{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.View.Components.Forms where

import Principium
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Model.Artwork
import WikiMusic.SSR.View.Components.Icons

mkSortingForm :: ViewVars -> SortOrder -> Text -> Text -> Html
mkSortingForm vv sortOrder action' fieldName = section
  $ H.form
  ! action (fromTextToAttributeValue action')
  ! method "POST"
  ! enctype "multipart/form-data"
  $ do
    select ! onchange "this.form.submit()" ! name (fromTextToAttributeValue fieldName) $ mapM_ mkOption entries
    noscript $ button ! type_ "submit" $ "submit"
  where
    mkOption :: (Text, Text) -> Html
    mkOption o =
      option
        H.!? (fst o == sortOrder ^. #value, selected "true")
        ! value (fromTextToAttributeValue $ fst o)
        $ text ("↕  " <> snd o)
    entries =
      [ ("display-name-asc", (^. #sortings % #alphabeticalAsc) |##| (vv ^. #language)),
        ("display-name-desc", (^. #sortings % #alphabeticalDesc) |##| (vv ^. #language)),
        ("created-at-asc", (^. #sortings % #createdAtAsc) |##| (vv ^. #language)),
        ("created-at-desc", (^. #sortings % #createdAtDesc) |##| (vv ^. #language)),
        ("last-edited-at-desc", (^. #sortings % #lastEditedAtDesc) |##| (vv ^. #language)),
        ("last-edited-at-asc", (^. #sortings % #lastEditedAtAsc) |##| (vv ^. #language))
      ]

requiredTextInput :: Text -> Text -> Html
requiredTextInput name' displayLabel = formInput name' (Just displayLabel) True "text" Nothing

searchInput :: Text -> Html
searchInput name' = formInput name' Nothing True "text" Nothing

requiredTextInput' :: Text -> Text -> Maybe Text -> Html
requiredTextInput' name' displayLabel = formInput name' (Just displayLabel) True "text"

requiredTextArea :: Text -> Text -> Html
requiredTextArea name' displayLabel = formArea name' (Just displayLabel) True False "text" Nothing

requiredTextArea' :: Text -> Text -> Maybe Text -> Html
requiredTextArea' name' displayLabel = formArea name' (Just displayLabel) True False "text"

requiredMonoArea :: Text -> Text -> Html
requiredMonoArea name' displayLabel = formArea name' (Just displayLabel) True True "text" Nothing

requiredMonoArea' :: Text -> Text -> Maybe Text -> Html
requiredMonoArea' name' displayLabel = formArea name' (Just displayLabel) True True "text"

optionalTextInput :: Text -> Text -> Html
optionalTextInput name' displayLabel = formInput name' (Just displayLabel) False "text" Nothing

optionalTextInput' :: Text -> Text -> Maybe Text -> Html
optionalTextInput' name' displayLabel = formInput name' (Just displayLabel) False "text"

optionalTextArea :: Text -> Text -> Html
optionalTextArea name' displayLabel = formArea name' (Just displayLabel) False False "text" Nothing

optionalTextArea' :: Text -> Text -> Maybe Text -> Html
optionalTextArea' name' displayLabel = formArea name' (Just displayLabel) False False "text"

optionalMonoArea :: Text -> Text -> Html
optionalMonoArea name' displayLabel = formArea name' (Just displayLabel) False True "text" Nothing

optionalMonoArea' :: Text -> Text -> Maybe Text -> Html
optionalMonoArea' name' displayLabel = formArea name' (Just displayLabel) False True "text"

requiredEmailInput :: Text -> Text -> Html
requiredEmailInput name' displayLabel = formInput name' (Just displayLabel) True "email" Nothing

requiredPasswordInput :: Text -> Text -> Html
requiredPasswordInput name' displayLabel = formInput name' (Just displayLabel) True "password" Nothing

optionalFileInput :: Text -> Text -> Html
optionalFileInput name' displayLabel =
  H.div $ do
    H.div $ H.label ! A.for name'' $ text displayLabel
    H.input
      ! class_ ""
      ! A.name name''
      ! A.id name''
      ! type_ "file"
  where
    name'' = fromTextToAttributeValue name'

formInput :: Text -> Maybe Text -> Bool -> AttributeValue -> Maybe Text -> Html
formInput name' displayLabel isRequired type' content' = H.div $ do
  H.div $ do
    mapM_ ((H.label ! A.for name'') . text) displayLabel
    mapM_ (\_ -> mapM_ (H.span ! class_ "color-error") (if isRequired then Just "*" else Nothing)) displayLabel
  H.input ! class_ "" H.!? (isRequired, required "") ! A.name name'' ! A.id name'' ! type_ type' ! A.value (fromTextToAttributeValue $ fromMaybe "" content')
  where
    name'' = fromTextToAttributeValue name'

formArea :: Text -> Maybe Text -> Bool -> Bool -> AttributeValue -> Maybe Text -> Html
formArea name' displayLabel isRequired isMono type' content' = H.div $ do
  H.div $ do
    mapM_ ((H.label ! A.for name'') . text) displayLabel
    mapM_ (H.span ! class_ "color-error") (if isRequired then Just "*" else Nothing)
  H.textarea
    ! class_ (if isMono then "font-mono big-mono-text-area font-size-small" else "font-sans")
    H.!? (isRequired, required "")
    ! A.name name''
    ! A.id name''
    ! type_ type'
    $ text (fromMaybe "" content')
  where
    name'' = fromTextToAttributeValue name'

deleteButton :: ViewVars -> Html
deleteButton vv =
  button ! A.class_ "background-error border-error align-self-flex-end" ! type_ "submit" $ text $ (^. #forms % #delete) |##| (vv ^. #language)

submitButton :: ViewVars -> Html
submitButton vv =
  button ! A.class_ "background-success border-success align-self-flex-end" ! type_ "submit" $ do
    H.span "✓"
    text $ (^. #forms % #submit) |##| (vv ^. #language)

submitButton' :: ViewVars -> Html
submitButton' vv =
  button ! A.class_ "background-success border-success" ! type_ "submit" $ do
    H.span "✓"
    text $ (^. #forms % #submit) |##| (vv ^. #language)

submitButtonNoText :: Html
submitButtonNoText =
  button ! A.class_ "background-accent border-accent button-for-input" ! type_ "submit" $ H.span $ H.small ! A.class_ "font-sans font-weight-300" $ "search"

dangerPostForm :: ViewVars -> Text -> Html -> Html
dangerPostForm vv action' =
  H.form
    ! class_ "margin-top-large flex direction-column align-items-flex-start"
    ! method "POST"
    ! action (fromTextToAttributeValue action')
    ! enctype "multipart/form-data"
    ! onsubmit (fromTextToAttributeValue $ "alert('" <> ((^. #more % #irreversibleAction) |##| (vv ^. #language)) <> "')")

postForm :: Text -> Html -> Html
postForm action' =
  H.form
    ! class_ "margin-top-large flex direction-column align-items-flex-start"
    ! method "POST"
    ! action (fromTextToAttributeValue action')
    ! enctype "multipart/form-data"

postForm' :: Text -> Text -> Html -> Html
postForm' action' class' =
  H.form
    ! class_ (fromTextToAttributeValue class')
    ! method "POST"
    ! action (fromTextToAttributeValue action')
    ! enctype "multipart/form-data"

searchForm :: Text -> Html -> Html
searchForm action' =
  H.form
    ! class_ (fromTextToAttributeValue "margin-top-medium flex direction-row align-items-flex-end no-gap")
    ! method "POST"
    ! action (fromTextToAttributeValue action')
    ! enctype "multipart/form-data"

entityArtworkForm :: ViewVars -> Text -> [Artwork] -> Html
entityArtworkForm vv path xs = section $ do
  unless (null xs) (hr >> (H.h2 ! A.id "edit-artwork" $ "Edit artwork"))
  H.div $ do
    let arts = sortBy (\x y -> compare (x ^. #orderValue) (y ^. #orderValue)) xs
    mapM_ (mkArtworkManager vv path) arts

mkArtworkManager :: ViewVars -> Text -> Artwork -> Html
mkArtworkManager vv path artwork = H.div $ do
  img
    ! class_ "object-cover"
    ! customAttribute "loading" "lazy"
    ! src (fromTextToAttributeValue $ artwork ^. #contentUrl)
  mapM_ (H.span . text) (artwork ^. #contentCaption)
  H.div $ do
    postForm ("/" <> path <> "/artworks/order/" <> uuidToText (artwork ^. #identifier)) $ do
      input ! type_ "hidden" ! name "orderValue" ! A.value (fromTextToAttributeValue plusOne)
      button ! class_ "small-button" ! type_ "submit" $ small . text $ plusOne
    postForm ("/" <> path <> "/artworks/order/" <> uuidToText (artwork ^. #identifier)) $ do
      input
        ! type_ "hidden"
        ! name "orderValue"
        ! A.value
          (fromTextToAttributeValue minusOne)
      button ! class_ "small-button" ! type_ "submit" $ small . text $ minusOne
  H.div
    ! A.class_ "flex direction-row justify-content-center gap-tiny"
    $ dangerPostForm
      vv
      ( "/"
          <> path
          <> "/artworks/delete/"
          <> uuidToText (artwork ^. #identifier)
      )
    $ button
    ! class_ "small-button"
    ! type_ "submit"
    $ small
    $ simpleIcon "❌" "delete"
  where
    plusOne = intToText $ artwork ^. #orderValue + 1
    minusOne =
      if artwork ^. #orderValue < 1
        then "0"
        else intToText $ artwork ^. #orderValue - 1

entityNewArtworkForm :: ViewVars -> Text -> UUID -> Html
entityNewArtworkForm vv path identifier = do
  H.h2 "New artwork"
  postForm ("/" <> path <> "/artworks/create/" <> uuidToText identifier) $ do
    requiredTextInput "contentUrl" "url"
    optionalTextInput "contentCaption" "caption"
    requiredTextInput' "orderValue" "position" (Just "0")
    submitButton vv
