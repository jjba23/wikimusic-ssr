{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Model.Api
  ( AuthToken (..),
    SortOrder (..),
    Include (..),
    UiMode (..),
    Language (..),
    Palette (..),
    authCookieName,
    localeCookieName,
    uiModeCookieName,
    ViewVars (..),
    artistSortingCookieName,
    songSortingCookieName,
    genreSortingCookieName,
    songAsciiSizeCookieName,
    paletteCookieName,
    SongAsciiSize (..),
  )
where

import Optics
import Relude

newtype AuthToken = AuthToken {value :: Text} deriving (Eq, Generic, Show)

newtype SortOrder = SortOrder {value :: Text} deriving (Eq, Generic, Show)

newtype Include = Include {value :: Text} deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''AuthToken
makeFieldLabelsNoPrefix ''SortOrder
makeFieldLabelsNoPrefix ''Include

newtype Language = Language {value :: Text} deriving (Eq, Generic, Show)

newtype UiMode = UiMode {value :: Text} deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Language
makeFieldLabelsNoPrefix ''UiMode

newtype SongAsciiSize = SongAsciiSize {value :: Text} deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''SongAsciiSize

newtype Palette = Palette {value :: Text} deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''Palette

localeCookieName :: Text
localeCookieName = "wikimusic-locale"

uiModeCookieName :: Text
uiModeCookieName = "wikimusic-ui-mode"

songAsciiSizeCookieName :: Text
songAsciiSizeCookieName = "wikimusic-song-ascii-size"

authCookieName :: Text
authCookieName = "wikimusic-auth"

artistSortingCookieName :: Text
artistSortingCookieName = "wikimusic-artist-sorting"

songSortingCookieName :: Text
songSortingCookieName = "wikimusic-song-sorting"

genreSortingCookieName :: Text
genreSortingCookieName = "wikimusic-genre-sorting"

paletteCookieName :: Text
paletteCookieName = "wikimusic-palette"

data ViewVars = ViewVars
  { language :: Language,
    uiMode :: UiMode,
    authToken :: AuthToken,
    songSorting :: SortOrder,
    artistSorting :: SortOrder,
    genreSorting :: SortOrder,
    songAsciiSize :: SongAsciiSize,
    palette :: Palette
  }
  deriving (Eq, Generic, Show)

makeFieldLabelsNoPrefix ''ViewVars
