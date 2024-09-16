{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.ArtistHtml
  ( artistListPage',
    artistDetailPage',
    artistCreatePage',
    artistEditPage',
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T
import Principium
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Interaction.Model.Artist
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.Components.Other
import WikiMusic.SSR.View.HtmlUtil

artistListPage' :: (MonadIO m) => Limit -> Offset -> Env -> ViewVars -> GetArtistsQueryResponse -> m Html
artistListPage' limit offset env vv xs =
  simplePage env vv (SimplePageTitle $ (^. #titles % #artistsPage) |##| (vv ^. #language)) $ do
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center"] $ do
      searchForm "/artists/search" $ do
        searchInput "searchInput"
        submitButtonNoText

      section $ do
        H.a ! href "/artists/create" $ button $ H.small "+ new artist"
        mkSortingForm vv (vv ^. #artistSorting) "/user-preferences/artist-sorting" "artist-sorting"
    section ! css cssCenteredCardGrid $ mapM_ (simpleEntityCard vv "artists") sortedXs
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center", "my-6"] $ do
      maybePrevPaginationButton vv limit offset (length (xs ^. #artists))
      maybeNextPaginationButton vv limit offset (length (xs ^. #artists))
  where
    sortedXs =
      mapMaybe
        (\identifier -> (xs ^. #artists) Principium.!? identifier)
        (xs ^. #sortOrder)

artistDetailPage' :: (MonadIO m) => Env -> ViewVars -> Artist -> m Html
artistDetailPage' env vv x = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #artistsPage) |##| (vv ^. #language)) $ do
    entityDetails vv "artists" x

artistCreatePage' :: (MonadIO m) => Env -> ViewVars -> m Html
artistCreatePage' env vv = do
  simplePage env vv (SimplePageTitle "Create artist") $ do
    section ! css' ["container", "mx-auto"] $ do
      postForm "/artists/create" $ do
        requiredTextInput "displayName" "artist name"
        optionalTextArea "description" "description"
        optionalTextInput "spotifyUrl" "spotify URL"
        optionalTextInput "youtubeUrl" "youtube URL"
        optionalTextInput "wikipediaUrl" "wikipedia URL"
        optionalTextInput "soundcloudUrl" "soundcloud URL"
        submitButton vv

artistEditPage' :: (MonadIO m) => Env -> ViewVars -> Artist -> m Html
artistEditPage' env vv artist = do
  simplePage env vv (SimplePageTitle "Edit artist") $ do
    section ! css' ["container", "mx-auto"] $ do
      postForm ("/artists/edit/" <> (T.pack . show $ artist ^. #identifier)) $ do
        requiredTextInput' "displayName" "artist name" (Just $ artist ^. #displayName)
        optionalTextArea' "description" "description" (artist ^. #description)
        optionalTextInput' "spotifyUrl" "spotify URL" (artist ^. #spotifyUrl)
        optionalTextInput' "youtubeUrl" "youtube URL" (artist ^. #youtubeUrl)
        optionalTextInput' "wikipediaUrl" "wikipedia URL" (artist ^. #wikipediaUrl)
        optionalTextInput' "soundcloudUrl" "soundcloud URL" (artist ^. #soundcloudUrl)
        submitButton vv

      entityArtworkForm vv "artists" (map (^. #artwork) . Map.elems $ artist ^. #artworks)
      hr
      entityNewArtworkForm vv "artists" (artist ^. #identifier)
