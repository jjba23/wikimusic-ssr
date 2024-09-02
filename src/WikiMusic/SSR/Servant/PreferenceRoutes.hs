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

import Control.Monad.Error.Class
import Data.Map qualified as Map
import Optics
import Relude
import Servant
import Servant.Multipart
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

setLanguageRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setLanguageRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (Map.fromList [(localeCookieName, wantedLanguage)])
  where
    wantedLanguage = fromForm multipartData "en" "locale"

setArtistSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setArtistSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/artists" maybeReferer) (Map.fromList [(artistSortingCookieName, artistSorting)])
  where
    artistSorting = fromForm multipartData "created-at-desc" "artist-sorting"

setGenreSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setGenreSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/genres" maybeReferer) (Map.fromList [(genreSortingCookieName, genreSorting)])
  where
    genreSorting = fromForm multipartData "created-at-desc" "genre-sorting"

setSongSortingRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setSongSortingRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (Map.fromList [(songSortingCookieName, songSorting)])
  where
    songSorting = fromForm multipartData "created-at-desc" "song-sorting"

setDarkModeRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setDarkModeRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (Map.fromList [(uiModeCookieName, mode)])
  where
    mode = fromForm multipartData "dark" "dark-mode"

setSongAsciiSizeRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setSongAsciiSizeRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (Map.fromList [(songAsciiSizeCookieName, songAsciiSize)])
  where
    songAsciiSize = fromForm multipartData "medium" "song-ascii-size"

setPaletteRoute :: (MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
setPaletteRoute env maybeReferer multipartData =
  setCookieRoute (env ^. #cfg % #cookie) (fromMaybe "/songs" maybeReferer) (Map.fromList [(paletteCookieName, palette')])
  where
    palette' = fromForm multipartData "mauve" "palette"
