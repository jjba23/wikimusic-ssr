{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.Html () where

import Data.Map qualified as Map
import Free.AlaCarte
import Principium
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.View.ArtistHtml
import WikiMusic.SSR.View.GenreHtml
import WikiMusic.SSR.View.OtherHtml
import WikiMusic.SSR.View.SongHtml

instance Exec View where
  -- artists
  execAlgebra (ArtistListPage env vv r next) =
    next =<< artistListPage' env vv r
  execAlgebra (ArtistDetailPage env vv r next) =
    next =<< artistDetailPage' env vv (Prelude.head . Map.elems $ r ^. #artists)
  execAlgebra (ArtistCreatePage env vv next) =
    next =<< artistCreatePage' env vv
  execAlgebra (ArtistEditPage env vv artist next) =
    next =<< artistEditPage' env vv artist
  -- genres
  execAlgebra (GenreListPage env vv r next) =
    next =<< genreListPage' env vv r
  execAlgebra (GenreDetailPage env vv r next) =
    next =<< genreDetailPage' env vv (Prelude.head . Map.elems $ r ^. #genres)
  execAlgebra (GenreCreatePage env vv next) =
    next =<< genreCreatePage' env vv
  execAlgebra (GenreEditPage env vv genre next) =
    next =<< genreEditPage' env vv genre
  -- songs
  execAlgebra (SongListPage env vv r next) =
    next =<< songListPage' env vv r
  execAlgebra (SongDetailPage env vv r next) =
    next =<< songDetailPage' env vv (Prelude.head . Map.elems $ r ^. #songs)
  execAlgebra (SongCreatePage env vv next) =
    next =<< songCreatePage' env vv
  execAlgebra (SongEditPage env vv song next) =
    next =<< songEditPage' env vv song
  execAlgebra (ErrorPage env vv maybeCode maybeMessage next) =
    next =<< errorPage' env vv maybeCode maybeMessage
  execAlgebra (LoginPage env vv next) =
    next =<< loginPage' env vv
  execAlgebra (RequestPasswordResetPage env vv next) =
    next =<< requestPasswordResetPage' env vv
  execAlgebra (DoPasswordResetPage env vv t next) =
    next =<< doPasswordResetPage' env vv t
  execAlgebra (InviteUsersPage env vv next) =
    next =<< inviteUserPage' env vv
