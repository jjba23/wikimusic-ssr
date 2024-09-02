{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module WikiMusic.SSR.Servant.SongRoutes where

import Control.Monad.Error.Class
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Maybe qualified
import Data.Text qualified as T
import Data.UUID (UUID)
import Free.AlaCarte
import Optics
import Relude
import Servant
import Servant.Multipart
import Text.Blaze.Html as Html
import WikiMusic.Interaction.Model.Song
import WikiMusic.Model.Other
import WikiMusic.Model.Song
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Free.Backend
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

songsRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m Html
songsRoute env cookie givenSortOrder limit offset searchInput = do
  maybeSongs <- case searchInput of
    Nothing ->
      liftIO
        $ exec @Backend
          ( getSongs
              env
              (vv ^. #authToken)
              (maybe (Limit 50) Limit limit)
              (maybe (Offset 0) Offset offset)
              sortOrder
              (Include {value = "artworks,comments,opinions,artists"})
          )
    Just search ->
      liftIO
        $ exec @Backend
          ( searchSongs
              env
              (vv ^. #authToken)
              search
              (maybe (Limit 50) Limit limit)
              (maybe (Offset 0) Offset offset)
              sortOrder
              (Include {value = "artworks,comments,opinions,artists"})
          )
  respondWithViewOrErr
    maybeSongs
    (exec @View . songListPage env vv)
  where
    vv = vvFromCookies cookie
    sortOrder = maybe (vv ^. #songSorting) SortOrder givenSortOrder

songRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
songRoute env cookie identifier = do
  maybeSongs <-
    liftIO
      $ exec @Backend
        ( getSong
            env
            (vv ^. #authToken)
            identifier
        )
  respondWithViewOrErr
    maybeSongs
    (exec @View . songDetailPage env vv)
  where
    vv = vvFromCookies cookie

songCreateRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
songCreateRoute env cookie = do
  liftIO $ exec @View (songCreatePage env vv)
  where
    vv = vvFromCookies cookie

songCreateFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
songCreateFormRoute env cookie multipartData = do
  createResult <- liftIO $ exec @Backend (createSong env (vv ^. #authToken) r)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ createResult)
  respondWithHttp
    httpFound
      { cause = Just "Created song!",
        headers = [withLocation "/songs"]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertSongsRequest
        { songs =
            [ InsertSongsRequestItem
                { displayName = fromForm multipartData "" "displayName",
                  spotifyUrl = maybeFromForm multipartData "spotifyUrl",
                  youtubeUrl = maybeFromForm multipartData "youtubeUrl",
                  soundcloudUrl = maybeFromForm multipartData "soundcloudUrl",
                  wikipediaUrl = maybeFromForm multipartData "wikipediaUrl",
                  description = maybeFromForm multipartData "description",
                  musicKey = maybeFromForm multipartData "musicKey",
                  musicTuning = maybeFromForm multipartData "musicTuning",
                  musicCreationDate = maybeFromForm multipartData "musicCreationDate",
                  albumName = maybeFromForm multipartData "albumName",
                  albumInfoLink = maybeFromForm multipartData "albumInfoLink"
                }
            ]
        }

songLikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
songLikeRoute env cookie maybeReferer identifier = do
  res <- liftIO $ exec @Backend (upsertSongOpinion env (vv ^. #authToken) r)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ res)
  respondWithHttp
    httpFound
      { cause = Just "Liked song!",
        headers = [withLocation (fromMaybe "/songs" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertSongOpinionsRequest
        { songOpinions =
            [ UpsertSongOpinionsRequestItem
                { songIdentifier = identifier,
                  isLike = True
                }
            ]
        }

songDislikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
songDislikeRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (upsertSongOpinion env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Disliked song!",
        headers = [withLocation (fromMaybe "/songs" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertSongOpinionsRequest
        { songOpinions =
            [ UpsertSongOpinionsRequestItem
                { songIdentifier = identifier,
                  isLike = False
                }
            ]
        }

songEditRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
songEditRoute env cookie identifier = do
  maybeSongs <-
    liftIO
      $ exec @Backend
        ( getSong
            env
            (vv ^. #authToken)
            identifier
        )
  let a = second (\x -> (head . Data.Maybe.fromJust . nonEmpty) $ Map.elems $ x ^. #songs) maybeSongs
  respondWithViewOrErr
    a
    (exec @View . songEditPage env vv)
  where
    vv = vvFromCookies cookie

songEditFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
songEditFormRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (editSong env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Edited song!",
        headers = [withLocation (maybe "/songs" (T.replace "/edit" "") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      SongDeltaRequest
        { songDeltas =
            [ SongDelta
                { identifier = identifier,
                  displayName = maybeFromForm multipartData "displayName",
                  spotifyUrl = maybeFromForm multipartData "spotifyUrl",
                  youtubeUrl = maybeFromForm multipartData "youtubeUrl",
                  soundcloudUrl = maybeFromForm multipartData "soundcloudUrl",
                  wikipediaUrl = maybeFromForm multipartData "wikipediaUrl",
                  description = maybeFromForm multipartData "description",
                  musicKey = maybeFromForm multipartData "musicKey",
                  musicTuning = maybeFromForm multipartData "musicTuning",
                  musicCreationDate = maybeFromForm multipartData "musicCreationDate",
                  albumName = maybeFromForm multipartData "albumName",
                  albumInfoLink = maybeFromForm multipartData "albumInfoLink"
                }
            ]
        }

searchSongRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
searchSongRoute _ multipartData = do
  respondWithHttp
    httpFound
      { cause = Just "Go to search songs page",
        headers = [withLocation newRoute]
      }
  where
    searchQuery = fromForm multipartData "" "searchInput"
    newRoute = "/songs?searchInput=" <> searchQuery

createSongArtworkRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
createSongArtworkRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (createSongArtwork env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Created song artwork!",
        headers = [withLocation (maybe "/songs" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertSongArtworksRequest
        { songArtworks =
            [ InsertSongArtworksRequestItem
                { songIdentifier = identifier,
                  orderValue = fromMaybe 0 $ readMaybe (T.unpack . fromMaybe "0" $ maybeFromForm multipartData "orderValue"),
                  contentUrl = fromMaybe "" $ maybeFromForm multipartData "contentUrl",
                  contentCaption = maybeFromForm multipartData "contentCaption"
                }
            ]
        }

songDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m a
songDeleteRoute env cookie identifier = do
  _ <- liftIO $ exec @Backend (deleteSong env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "Deleted song!",
        headers = [("Location", "/songs")]
      }
  where
    vv = vvFromCookies cookie

songArtworkDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
songArtworkDeleteRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (deleteSongArtwork env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "Deleted song artwork!",
        headers = [withLocation (maybe "/songs" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie

updateSongArtworkOrderRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
updateSongArtworkOrderRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (updateSongArtworkOrder env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Updated song artwork!",
        headers = [withLocation (maybe "/songs" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      SongArtworkOrderUpdateRequest
        { songArtworkOrders =
            [ SongArtworkOrderUpdate
                { identifier = identifier,
                  orderValue =
                    fromMaybe 0
                      $ readMaybe (T.unpack . fromMaybe "0" $ maybeFromForm multipartData "orderValue")
                }
            ]
        }

songContentCreateFormRoute ::
  (MonadIO m, MonadError ServerError m) =>
  Env ->
  Maybe Text ->
  Maybe Text ->
  UUID ->
  MultipartData tag ->
  m b
songContentCreateFormRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (createSongContents env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Edited song!",
        headers = [withLocation (maybe "/songs" (T.replace "/edit" "" . (<> "#edit-contents")) maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertSongContentsRequest
        { songContents =
            [ InsertSongContentsRequestItem
                { songIdentifier = identifier,
                  versionName = fromMaybe "" $ maybeFromForm multipartData "versionName",
                  instrumentType = fromMaybe "" $ maybeFromForm multipartData "instrumentType",
                  asciiLegend = maybeFromForm multipartData "asciiLegend",
                  asciiContents = maybeFromForm multipartData "asciiContents",
                  pdfContents = maybeFromForm multipartData "pdfContents",
                  guitarProContents = maybeFromForm multipartData "guitarProContents"
                }
            ]
        }

songContentEditFormRoute ::
  (MonadIO m, MonadError ServerError m) =>
  Env ->
  Maybe Text ->
  Maybe Text ->
  p ->
  UUID ->
  MultipartData tag ->
  m b
songContentEditFormRoute env cookie maybeReferer _ songContentIdentifier multipartData = do
  _ <- liftIO $ exec @Backend (editSongContents env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Edited song!",
        headers = [withLocation (maybe "/songs" (T.replace "/edit" "") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      SongContentDeltaRequest
        { songContentDeltas =
            [ SongContentDelta
                { identifier = songContentIdentifier,
                  versionName = fromMaybe "" $ maybeFromForm multipartData "versionName",
                  instrumentType = maybeFromForm multipartData "instrumentType",
                  asciiLegend = maybeFromForm multipartData "asciiLegend",
                  asciiContents = maybeFromForm multipartData "asciiContents",
                  pdfContents = maybeFromForm multipartData "pdfContents",
                  guitarProContents = maybeFromForm multipartData "guitarProContents"
                }
            ]
        }

songContentDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> UUID -> Maybe Text -> Maybe Text -> m a
songContentDeleteRoute env identifier cookie maybeReferer = do
  _ <- liftIO $ exec @Backend (deleteSongContents env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "Deleted song contents!",
        headers = [withLocation (fromMaybe "/songs" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
