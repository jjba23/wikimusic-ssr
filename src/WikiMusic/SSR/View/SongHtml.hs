{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.SongHtml
  ( songListPage',
    songDetailPage',
    songCreatePage',
    songEditPage',
  )
where

import Principium
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Song hiding (show)
import WikiMusic.SSR.View.Components.DetailList
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.Components.Other
import WikiMusic.SSR.View.HtmlUtil

songListPage' :: (MonadIO m) => Limit -> Offset -> Env -> ViewVars -> GetSongsQueryResponse -> m Html
songListPage' limit offset env vv xs =
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ do
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center"] $ do
      searchForm "/songs/search" $ do
        searchInput "searchInput"
        submitButtonNoText
      section $ do
        H.a ! href "/songs/create" $ button $ H.small "+ new song"
        mkSortingForm vv (vv ^. #songSorting) "/user-preferences/song-sorting" "song-sorting"
    --
    section ! css cssCenteredCardGrid $ mapM_ (simpleEntityCard vv "songs") sortedXs
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center", "my-6"] $ do
      maybePrevPaginationButton vv limit offset (length (xs ^. #songs))
      maybeNextPaginationButton vv limit offset (length (xs ^. #songs))
  where
    sortedXs =
      mapMaybe
        (\identifier -> (xs ^. #songs) Principium.!? identifier)
        (xs ^. #sortOrder)

songDetailPage' :: (MonadIO m) => Env -> ViewVars -> Song -> m Html
songDetailPage' env vv x = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ do
    entityDetails vv "songs" x
    songDetails vv x
    H.form ! css' ["text-center"] ! action "/user-preferences/song-ascii-size" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! css (cssSelect vv) ! onchange "this.form.submit()" ! type_ "checkbox" ! name "song-ascii-size" ! A.id "song-ascii-size" $ do
        mapM_
          ( \size' ->
              let mkOption = option H.!? ((vv ^. #songAsciiSize % #value) == size', selected "true") ! value (textToAttrValue size')
               in mkOption . text $ size'
          )
          fontSizes
      noscript $ button ! type_ "submit" $ "submit"
    section ! css' ["container", "mx-auto"] $ do
      mapM_ (mkVersion vv) (x ^. #contents)
  where
    fontSizes :: [Text]
    fontSizes = ["xs", "sm", "md", "lg", "xl"]

songDetails :: ViewVars -> Song -> Html
songDetails vv x = do
  section $ detailList $ do
    mapM_
      (monoDetailListEntry vv ((^. #more % #musicTuning) |##| (vv ^. #language)) . text)
      (x ^. #musicTuning)
    mapM_
      (detailListEntry vv ((^. #more % #musicKey) |##| (vv ^. #language)) . text)
      (x ^. #musicKey)
    mapM_
      (detailListEntry vv ((^. #more % #musicCreationDate) |##| (vv ^. #language)) . text)
      (x ^. #musicCreationDate)
    mapM_
      (detailListEntry vv ((^. #more % #albumName) |##| (vv ^. #language)) . text)
      (x ^. #albumName)
    mapM_
      (detailListEntry vv ((^. #more % #albumInfoLink) |##| (vv ^. #language)) . text)
      (x ^. #albumInfoLink)

mkVersion :: ViewVars -> SongContent -> Html
mkVersion vv v = do
  hr
  H.article ! css' ["my-6"] $ do
    (h3 ! css' ["text-xl", "font-bold"]) . text $ (v ^. #versionName) <> " " <> (v ^. #instrumentType)

    detailList $ do
      mapM_
        (detailListEntry vv ((^. #more % #lastEditedAt) |##| (vv ^. #language)))
        (show <$> v ^. #lastEditedAt)
      detailListEntry vv ((^. #more % #createdAt) |##| (vv ^. #language)) (show $ v ^. #createdAt)
      monoDetailListEntry vv ((^. #more % #createdBy) |##| (vv ^. #language)) (show $ v ^. #createdBy)

    mapM_
      ( \asciiLegend -> details ! css (cssDetails vv) ! open "" $ do
          H.summary ! css cssSummary $ "ASCII Legend"
          (H.pre ! class_ (textToAttrValue $ "text-" <> (vv ^. #songAsciiSize % #value))) . text $ asciiLegend
      )
      (v ^. #asciiLegend)
    mapM_
      ( \asciiContents -> details ! css (cssDetails vv) ! open "" $ do
          H.summary ! css cssSummary $ "ASCII Content"
          (H.pre ! class_ (textToAttrValue $ "text-" <> (vv ^. #songAsciiSize % #value))) . text $ asciiContents
      )
      (v ^. #asciiContents)
    mapM_
      ( \pdfContents ->
          when (pdfContents /= "data:application/octet-stream;base64,") $ do
            details ! css (cssDetails vv) ! open "" $ do
              H.summary ! css cssSummary $ "PDF Content"
              H.iframe
                ! css' ["w-full", "h-full", "block"]
                ! customAttribute "loading" "lazy"
                ! customAttribute "allowed" ""
                ! customAttribute "allowfullscreen" ""
                ! customAttribute "referrerpolicy" "noreferrer"
                ! A.src (textToAttrValue pdfContents)
                $ ""
      )
      (v ^. #pdfContents)

songCreatePage' :: (MonadIO m) => Env -> ViewVars -> m Html
songCreatePage' env vv = do
  simplePage env vv (SimplePageTitle "Create song") $ section $ do
    postForm "/songs/create" $ do
      requiredTextInput "displayName" "song name"
      optionalTextArea "description" "description"
      optionalTextInput "spotifyUrl" "spotify URL"
      optionalTextInput "youtubeUrl" "youtube URL"
      optionalTextInput "wikipediaUrl" "wikipedia URL"
      optionalTextInput "soundcloudUrl" "soundcloud URL"
      optionalTextInput "musicKey" "music key"
      optionalTextInput "musicTuning" "tuning"
      optionalTextInput "musicCreationDate" "date composed"
      optionalTextInput "albumName" "album name"
      optionalTextInput "albumInfoLink" "about the album"
      submitButton vv

songEditPage' :: (MonadIO m) => Env -> ViewVars -> Song -> m Html
songEditPage' env vv song = do
  simplePage env vv (SimplePageTitle "Edit song") $ section $ do
    postForm ("/songs/edit/" <> uuidToText (song ^. #identifier)) $ do
      requiredTextInput' "displayName" "song name" (Just $ song ^. #displayName)
      optionalTextArea' "description" "description" (song ^. #description)
      optionalTextInput' "spotifyUrl" "spotify URL" (song ^. #spotifyUrl)
      optionalTextInput' "youtubeUrl" "youtube URL" (song ^. #youtubeUrl)
      optionalTextInput' "wikipediaUrl" "wikipedia URL" (song ^. #wikipediaUrl)
      optionalTextInput' "soundcloudUrl" "soundcloud URL" (song ^. #soundcloudUrl)
      optionalTextInput' "musicKey" "music key" (song ^. #musicKey)
      optionalTextInput' "musicTuning" "tuning" (song ^. #musicTuning)
      optionalTextInput' "musicCreationDate" "date composed" (song ^. #musicCreationDate)
      optionalTextInput' "albumName" "album name" (song ^. #albumName)
      optionalTextInput' "albumInfoLink" "about the album" (song ^. #albumInfoLink)
      submitButton vv

    entityArtworkForm vv "songs" (map (^. #artwork) . mapElems $ song ^. #artworks)
    hr
    entityNewArtworkForm vv "songs" (song ^. #identifier)
    mapM_ (\c -> hr >> songContentsEditForm env vv (song ^. #identifier) c) (mapElems $ song ^. #contents)
    hr
    H.h2 "Create contents"
    songContentsCreateForm vv (song ^. #identifier)
    hr
    H.h2 "Artist <> Song"

    mapM_
      ( \art -> do
          H.h4 . text $ art
          -- dangerPostForm vv ("/songs/" <> (T.pack . Relude.show $ song ^. #identifier) <> "/artists/" <> (T.pack . Relude.show $ artistIdentifier) <> "/delete") $ do
          --   deleteButton vv
      )
      (mapElems $ song ^. #artists)
    songArtistForm vv (song ^. #identifier)

songArtistForm :: ViewVars -> UUID -> Html
songArtistForm vv songIdentifier = do
  postForm ("/songs/" <> uuidToText songIdentifier <> "/artists") $ do
    requiredTextInput "identifier" "artist identifier (UUID)"
    submitButton vv

songContentsCreateForm :: ViewVars -> UUID -> Html
songContentsCreateForm vv songIdentifier = do
  postForm ("/songs/" <> (packText . show $ songIdentifier) <> "/contents") $ do
    requiredTextInput "versionName" "version name"
    requiredTextInput "instrumentType" "instrument type"
    optionalMonoArea "asciiLegend" "ascii legend"
    optionalMonoArea "asciiContents" "ascii contents"
    optionalFileInput "pdfContents" "pdf file"
    optionalFileInput "guitarProContents" "guitar pro file"
    submitButton vv

songContentsEditForm :: Env -> ViewVars -> UUID -> SongContent -> Html
songContentsEditForm _ vv songIdentifier content' = do
  H.h3 "Edit contents"
  dangerPostForm
    vv
    ( "/songs/contents/"
        <> (packText . show $ content' ^. #identifier)
        <> "/delete"
    )
    $ do
      deleteButton vv
  postForm
    ( "/songs/"
        <> (packText . show $ songIdentifier)
        <> "/contents/"
        <> (packText . show $ content' ^. #identifier)
    )
    $ do
      requiredTextInput' "versionName" "version name" (Just $ content' ^. #versionName)
      requiredTextInput' "instrumentType" "instrument type" (Just $ content' ^. #instrumentType)
      optionalMonoArea' "asciiLegend" "ascii legend" (content' ^. #asciiLegend)
      optionalMonoArea' "asciiContents" "ascii contents" (content' ^. #asciiContents)
      submitButton vv
