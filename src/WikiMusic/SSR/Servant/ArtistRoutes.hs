{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.Servant.ArtistRoutes where

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
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Model.Artist
import WikiMusic.Model.Other
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Free.Backend
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

debugPrint :: (MonadIO m, Show p) => p -> m ()
debugPrint a = liftIO $ BL.putStr . fromString . Relude.show $ a

artistsRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m Html
artistsRoute env cookie givenSortOrder limit offset searchInput = do
  maybeArtists <- case searchInput of
    Nothing ->
      liftIO
        $ exec @Backend
          ( getArtists
              env
              (vv ^. #authToken)
              (maybe (Limit 50) Limit limit)
              (maybe (Offset 0) Offset offset)
              sortOrder
              (Include {value = "artworks,comments,opinions"})
          )
    Just search ->
      liftIO
        $ exec @Backend
          ( searchArtists
              env
              (vv ^. #authToken)
              search
              (maybe (Limit 50) Limit limit)
              (maybe (Offset 0) Offset offset)
              sortOrder
              (Include {value = "artworks,comments,opinions"})
          )
  respondWithViewOrErr
    maybeArtists
    (exec @View . artistListPage env vv)
  where
    vv = vvFromCookies cookie
    sortOrder = maybe (vv ^. #artistSorting) SortOrder givenSortOrder

artistRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
artistRoute env cookie identifier = do
  maybeArtists <-
    liftIO
      $ exec @Backend
        ( getArtist
            env
            (vv ^. #authToken)
            identifier
        )
  debugPrint maybeArtists
  respondWithViewOrErr
    maybeArtists
    (exec @View . artistDetailPage env vv)
  where
    vv = vvFromCookies cookie

artistCreateRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
artistCreateRoute env cookie = do
  liftIO $ exec @View (artistCreatePage env vv)
  where
    vv = vvFromCookies cookie

artistCreateFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
artistCreateFormRoute env cookie multipartData = do
  createResult <- liftIO $ exec @Backend (createArtist env (vv ^. #authToken) r)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ createResult)
  respondWithHttp
    httpFound
      { cause = Just "Created artist!",
        headers = [withLocation "/artists"]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertArtistsRequest
        { artists =
            [ InsertArtistsRequestItem
                { displayName = fromForm multipartData "" "displayName",
                  spotifyUrl = maybeFromForm multipartData "spotifyUrl",
                  youtubeUrl = maybeFromForm multipartData "youtubeUrl",
                  soundcloudUrl = maybeFromForm multipartData "soundcloudUrl",
                  wikipediaUrl = maybeFromForm multipartData "wikipediaUrl",
                  description = maybeFromForm multipartData "description"
                }
            ]
        }

artistLikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
artistLikeRoute env cookie maybeReferer identifier = do
  res <- liftIO $ exec @Backend (upsertArtistOpinion env (vv ^. #authToken) r)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ res)
  respondWithHttp
    httpFound
      { cause = Just "Liked artist!",
        headers = [withLocation (fromMaybe "/artists" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertArtistOpinionsRequest
        { artistOpinions =
            [ UpsertArtistOpinionsRequestItem
                { artistIdentifier = identifier,
                  isLike = True
                }
            ]
        }

artistDislikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
artistDislikeRoute env cookie maybeReferer identifier = do
  res <- liftIO $ exec @Backend (upsertArtistOpinion env (vv ^. #authToken) r)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ res)
  respondWithHttp
    httpFound
      { cause = Just "Disliked artist!",
        headers = [withLocation (fromMaybe "/artists" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertArtistOpinionsRequest
        { artistOpinions =
            [ UpsertArtistOpinionsRequestItem
                { artistIdentifier = identifier,
                  isLike = False
                }
            ]
        }

artistEditRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
artistEditRoute env cookie identifier = do
  maybeArtists <-
    liftIO
      $ exec @Backend
        ( getArtist
            env
            (vv ^. #authToken)
            identifier
        )
  let a = second (\x -> (head . Data.Maybe.fromJust . nonEmpty) $ Map.elems $ x ^. #artists) maybeArtists
  respondWithViewOrErr
    a
    (exec @View . artistEditPage env vv)
  where
    vv = vvFromCookies cookie

artistEditFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
artistEditFormRoute env cookie maybeReferer identifier multipartData = do
  editResult <- liftIO $ exec @Backend (editArtist env (vv ^. #authToken) r)
  _ <- debugPrint editResult
  respondWithHttp
    httpFound
      { cause = Just "Updated artist!",
        headers = [withLocation (maybe "/artists" (T.replace "/edit" "") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      ArtistDeltaRequest
        { artistDeltas =
            [ ArtistDelta
                { identifier = identifier,
                  displayName = maybeFromForm multipartData "displayName",
                  spotifyUrl = maybeFromForm multipartData "spotifyUrl",
                  youtubeUrl = maybeFromForm multipartData "youtubeUrl",
                  soundcloudUrl = maybeFromForm multipartData "soundcloudUrl",
                  wikipediaUrl = maybeFromForm multipartData "wikipediaUrl",
                  description = maybeFromForm multipartData "description"
                }
            ]
        }

searchArtistRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
searchArtistRoute _ multipartData =
  respondWithHttp
    httpFound
      { cause = Just "Go to search artist page!",
        headers = [withLocation newRoute]
      }
  where
    searchData = fromForm multipartData "" "searchInput"
    newRoute = "/artists?searchInput=" <> searchData

createArtistArtworkRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
createArtistArtworkRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (createArtistArtwork env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Created artist!",
        headers = [withLocation (maybe "/artists" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertArtistArtworksRequest
        { artistArtworks =
            [ InsertArtistArtworksRequestItem
                { artistIdentifier = identifier,
                  orderValue = fromMaybe 0 $ readMaybe (T.unpack . fromMaybe "0" $ maybeFromForm multipartData "orderValue"),
                  contentUrl = fromMaybe "" $ maybeFromForm multipartData "contentUrl",
                  contentCaption = maybeFromForm multipartData "contentCaption"
                }
            ]
        }

artistDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m a
artistDeleteRoute env cookie identifier = do
  res <- liftIO $ exec @Backend (deleteArtist env (vv ^. #authToken) identifier)
  _ <- liftIO $ BL.putStr (fromString . Relude.show $ res)
  respondWithHttp
    httpFound
      { cause = Just "Deleted artist!",
        headers = [withLocation "/artists"]
      }
  where
    vv = vvFromCookies cookie

artistArtworkDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
artistArtworkDeleteRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (deleteArtistArtwork env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "",
        headers = [withLocation (maybe "/artists" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie

updateArtistArtworkOrderRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
updateArtistArtworkOrderRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (updateArtistArtworkOrder env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Updated artist!",
        headers = [withLocation (maybe "/artists" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      ArtistArtworkOrderUpdateRequest
        { artistArtworkOrders =
            [ ArtistArtworkOrderUpdate
                { identifier = identifier,
                  orderValue = fromMaybe 0 $ readMaybe (T.unpack . fromMaybe "0" $ maybeFromForm multipartData "orderValue")
                }
            ]
        }
