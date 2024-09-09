{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.GenreHtml
  ( genreListPage',
    genreDetailPage',
    genreCreatePage',
    genreEditPage',
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T
import Principium
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Interaction.Model.Genre
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.Components.Other
import WikiMusic.SSR.View.HtmlUtil

genreListPage' :: (MonadIO m) => Env -> ViewVars -> GetGenresQueryResponse -> m Html
genreListPage' env vv xs =
  simplePage env vv (SimplePageTitle $ (^. #titles % #genresPage) |##| (vv ^. #language)) $ do
    section $ do
      searchForm "/genres/search" $ do
        searchInput "searchInput"
        submitButtonNoText
      section ! class_ "flex direction-row justify-content-center gap-small align-items-baseline" $ do
        H.a ! href "/genres/create" $ button $ H.small "+ new genre"
        mkSortingForm vv (vv ^. #genreSorting) "/user-preferences/genre-sorting" "genre-sorting"
      section ! class_ "flex direction-row justify-content-center gap-small" $ mapM_ (simpleEntityCard vv "genres") sortedXs
  where
    sortedXs =
      mapMaybe
        (\identifier -> (xs ^. #genres) Map.!? identifier)
        (xs ^. #sortOrder)

genreDetailPage' :: (MonadIO m) => Env -> ViewVars -> Genre -> m Html
genreDetailPage' env vv x = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #genresPage) |##| (vv ^. #language)) $ do
    entityDetails vv "genres" x

genreCreatePage' :: (MonadIO m) => Env -> ViewVars -> m Html
genreCreatePage' env vv =
  simplePage env vv (SimplePageTitle "Create genre") $ do
    section $ do
      postForm "/genres/create" $ do
        requiredTextInput "displayName" "genre name"
        optionalTextArea "description" "description"
        optionalTextInput "spotifyUrl" "spotify URL"
        optionalTextInput "youtubeUrl" "youtube URL"
        optionalTextInput "wikipediaUrl" "wikipedia URL"
        optionalTextInput "soundcloudUrl" "soundcloud URL"
        submitButton vv

genreEditPage' :: (MonadIO m) => Env -> ViewVars -> Genre -> m Html
genreEditPage' env vv genre =
  simplePage env vv (SimplePageTitle "Edit genre") $ do
    section $ do
      postForm ("/genres/edit/" <> (T.pack . show $ genre ^. #identifier)) $ do
        requiredTextInput' "displayName" "genre name" (Just $ genre ^. #displayName)
        optionalTextArea' "description" "description" (genre ^. #description)
        optionalTextInput' "spotifyUrl" "spotify URL" (genre ^. #spotifyUrl)
        optionalTextInput' "youtubeUrl" "youtube URL" (genre ^. #youtubeUrl)
        optionalTextInput' "wikipediaUrl" "wikipedia URL" (genre ^. #wikipediaUrl)
        optionalTextInput' "soundcloudUrl" "soundcloud URL" (genre ^. #soundcloudUrl)

        submitButton vv
    entityArtworkForm vv "genres" (Relude.map (^. #artwork) . Map.elems $ genre ^. #artworks)
    hr
    entityNewArtworkForm vv "genres" (genre ^. #identifier)
