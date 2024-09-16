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

import Principium
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Interaction.Model.Genre
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.Components.Other
import WikiMusic.SSR.View.HtmlUtil

genreListPage' :: (MonadIO m) => Limit -> Offset -> Env -> ViewVars -> GetGenresQueryResponse -> m Html
genreListPage' limit offset env vv xs =
  simplePage env vv (SimplePageTitle $ (^. #titles % #genresPage) |##| (vv ^. #language)) $ do
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center"] $ do
      searchForm "/genres/search" $ do
        searchInput "searchInput"
        submitButtonNoText
      section $ do
        H.a ! href "/genres/create" $ button $ H.small "+ new genre"
        mkSortingForm vv (vv ^. #genreSorting) "/user-preferences/genre-sorting" "genre-sorting"
    --
    section ! css cssCenteredCardGrid $ mapM_ (simpleEntityCard vv "genres") sortedXs
    section ! css' ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center", "items-center", "my-6"] $ do
      maybePrevPaginationButton vv limit offset (length (xs ^. #genres))
      maybeNextPaginationButton vv limit offset (length (xs ^. #genres))
  where
    sortedXs =
      mapMaybe
        (\identifier -> (xs ^. #genres) Principium.!? identifier)
        (xs ^. #sortOrder)

genreDetailPage' :: (MonadIO m) => Env -> ViewVars -> Genre -> m Html
genreDetailPage' env vv x = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #genresPage) |##| (vv ^. #language)) $ do
    entityDetails vv "genres" x

genreCreatePage' :: (MonadIO m) => Env -> ViewVars -> m Html
genreCreatePage' env vv =
  simplePage env vv (SimplePageTitle "Create genre") $ do
    section ! css' ["container", "mx-auto"] $ do
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
    section ! css' ["container", "mx-auto"] $ do
      postForm ("/genres/edit/" <> (packText . show $ genre ^. #identifier)) $ do
        requiredTextInput' "displayName" "genre name" (Just $ genre ^. #displayName)
        optionalTextArea' "description" "description" (genre ^. #description)
        optionalTextInput' "spotifyUrl" "spotify URL" (genre ^. #spotifyUrl)
        optionalTextInput' "youtubeUrl" "youtube URL" (genre ^. #youtubeUrl)
        optionalTextInput' "wikipediaUrl" "wikipedia URL" (genre ^. #wikipediaUrl)
        optionalTextInput' "soundcloudUrl" "soundcloud URL" (genre ^. #soundcloudUrl)

        submitButton vv
      entityArtworkForm vv "genres" (map (^. #artwork) . mapElems $ genre ^. #artworks)
      hr
      entityNewArtworkForm vv "genres" (genre ^. #identifier)
