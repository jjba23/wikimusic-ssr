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

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UUID (UUID)
import Optics
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Song
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.View.Components.DetailList
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.Components.Other
import WikiMusic.SSR.View.HtmlUtil

songListPage' :: (MonadIO m) => Env -> ViewVars -> GetSongsQueryResponse -> m Html
songListPage' env vv xs =
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ do
    searchForm "/songs/search" $ do
      searchInput "searchInput"
      submitButtonNoText
    section ! class_ "flex direction-row justify-content-center gap-small align-items-baseline" $ do
      H.a ! href "/songs/create" $ button $ H.small "+ new song"
      mkSortingForm vv (vv ^. #songSorting) "/user-preferences/song-sorting" "song-sorting"
    section ! class_ "flex direction-row justify-content-center gap-small" $ mapM_ (simpleEntityCard vv "songs") sortedXs
  where
    sortedXs =
      mapMaybe
        (\identifier -> (xs ^. #songs) Map.!? identifier)
        (xs ^. #sortOrder)

songDetailPage' :: (MonadIO m) => Env -> ViewVars -> Song -> m Html
songDetailPage' env vv x = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ do
    entityDetails vv "songs" x
    songDetails vv x
    H.div $ H.form ! action "/user-preferences/song-ascii-size" ! method "POST" ! enctype "multipart/form-data" $ do
      select ! onchange "this.form.submit()" ! type_ "checkbox" ! name "song-ascii-size" ! A.id "song-ascii-size" $ do
        mapM_
          ( \size' ->
              let mkOption = option !? ((vv ^. #songAsciiSize % #value) == size', selected "true") ! value (fromString . T.unpack $ size')
               in mkOption . text $ size'
          )
          fontSizes
      noscript $ button ! type_ "submit" $ "submit"
    section $ do
      mapM_ (mkVersion vv) (x ^. #contents)
  where
    fontSizes :: [Text]
    fontSizes = ["xx-small", "x-small", "small", "medium", "large", "larger", "x-large", "xx-large"]

songDetails :: ViewVars -> Song -> Html
songDetails vv x = do
  section $ detailList $ do
    mapM_
      (detailListEntry ((^. #more % #musicTuning) |##| (vv ^. #language)) . text)
      (x ^. #musicTuning)
    mapM_
      (detailListEntry ((^. #more % #musicKey) |##| (vv ^. #language)) . text)
      (x ^. #musicKey)
    mapM_
      (detailListEntry ((^. #more % #musicCreationDate) |##| (vv ^. #language)) . text)
      (x ^. #musicCreationDate)
    mapM_
      (detailListEntry ((^. #more % #albumName) |##| (vv ^. #language)) . text)
      (x ^. #albumName)
    mapM_
      (detailListEntry ((^. #more % #albumInfoLink) |##| (vv ^. #language)) . text)
      (x ^. #albumInfoLink)

mkVersion :: ViewVars -> SongContent -> Html
mkVersion vv v = H.article $ do
  hr ! class_ "margin-top-medium"
  h3 . text $ (v ^. #versionName) <> " " <> (v ^. #instrumentType)

  detailList $ do
    mapM_
      (detailListEntry ((^. #more % #lastEditedAt) |##| (vv ^. #language)))
      (Relude.show <$> v ^. #lastEditedAt)
    detailListEntry ((^. #more % #createdAt) |##| (vv ^. #language)) (Relude.show $ v ^. #createdAt)
    detailListEntry ((^. #more % #createdBy) |##| (vv ^. #language)) (Relude.show $ v ^. #createdBy)

  mapM_
    ( \asciiLegend -> details ! open "" $ do
        H.summary "ASCII Legend"
        (H.pre ! class_ (fromString . T.unpack $ "font-size-" <> (vv ^. #songAsciiSize % #value))) . text $ asciiLegend
    )
    (v ^. #asciiLegend)
  mapM_
    ( \asciiContents -> details ! open "" $ do
        H.summary "ASCII Content"
        (H.pre ! class_ (fromString . T.unpack $ "font-size-" <> (vv ^. #songAsciiSize % #value))) . text $ asciiContents
    )
    (v ^. #asciiContents)
  mapM_
    ( \pdfContents -> details ! open "" $ do
        when (pdfContents /= "data:application/octet-stream;base64,") $ do
          H.summary "PDF Content"
          H.iframe
            ! customAttribute "loading" "lazy"
            ! customAttribute "allowed" ""
            ! customAttribute "allowfullscreen" ""
            ! customAttribute "referrerpolicy" "noreferrer"
            ! A.src (fromString . T.unpack $ pdfContents)
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
    postForm ("/songs/edit/" <> (T.pack . Relude.show $ song ^. #identifier)) $ do
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

    entityArtworkForm vv "songs" (Relude.map (^. #artwork) . Map.elems $ song ^. #artworks)
    hr
    entityNewArtworkForm vv "songs" (song ^. #identifier)
    mapM_ (\c -> hr >> songContentsEditForm env vv (song ^. #identifier) c) (Map.elems $ song ^. #contents)
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
      (Map.elems $ song ^. #artists)
    songArtistForm vv (song ^. #identifier)

songArtistForm :: ViewVars -> UUID -> Html
songArtistForm vv songIdentifier = do
  postForm ("/songs/" <> (T.pack . Relude.show $ songIdentifier) <> "/artists") $ do
    requiredTextInput "identifier" "artist identifier (UUID)"
    submitButton vv

songContentsCreateForm :: ViewVars -> UUID -> Html
songContentsCreateForm vv songIdentifier = do
  postForm ("/songs/" <> (T.pack . Relude.show $ songIdentifier) <> "/contents") $ do
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
        <> (T.pack . Relude.show $ content' ^. #identifier)
        <> "/delete"
    )
    $ do
      deleteButton vv
  postForm
    ( "/songs/"
        <> (T.pack . Relude.show $ songIdentifier)
        <> "/contents/"
        <> (T.pack . Relude.show $ content' ^. #identifier)
    )
    $ do
      requiredTextInput' "versionName" "version name" (Just $ content' ^. #versionName)
      requiredTextInput' "instrumentType" "instrument type" (Just $ content' ^. #instrumentType)
      optionalMonoArea' "asciiLegend" "ascii legend" (content' ^. #asciiLegend)
      optionalMonoArea' "asciiContents" "ascii contents" (content' ^. #asciiContents)
      submitButton vv