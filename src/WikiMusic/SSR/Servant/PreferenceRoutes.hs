{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.Servant.PreferenceRoutes
  ( setLanguageRoute,
    setArtistSortingRoute,
    setGenreSortingRoute,
    setSongSortingRoute,
    setDarkModeRoute,
    setSongAsciiSizeRoute,
    setPaletteRoute,
  )
where

import Principium
import Servant
import Servant.Multipart
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

setLanguageRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setLanguageRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (mapFromList [(localeCookieName, wantedLanguage)])
  where
    wantedLanguage = fromForm multipartData "en" "locale"

setArtistSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setArtistSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/artists" maybeReferer) (mapFromList [(artistSortingCookieName, artistSorting)])
  where
    artistSorting = fromForm multipartData "created-at-desc" "artist-sorting"

setGenreSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setGenreSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/genres" maybeReferer) (mapFromList [(genreSortingCookieName, genreSorting)])
  where
    genreSorting = fromForm multipartData "created-at-desc" "genre-sorting"

setSongSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setSongSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (mapFromList [(songSortingCookieName, songSorting)])
  where
    songSorting = fromForm multipartData "created-at-desc" "song-sorting"

setDarkModeRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setDarkModeRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (mapFromList [(uiModeCookieName, mode)])
  where
    mode = fromForm multipartData "dark" "dark-mode"

setSongAsciiSizeRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setSongAsciiSizeRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (mapFromList [(songAsciiSizeCookieName, songAsciiSize)])
  where
    songAsciiSize = fromForm multipartData "medium" "song-ascii-size"

setPaletteRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setPaletteRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (mapFromList [(paletteCookieName, palette')])
  where
    palette' = fromForm multipartData "purple" "palette"
