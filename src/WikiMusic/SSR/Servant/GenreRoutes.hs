{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.Servant.GenreRoutes where

import Principium
import Servant
import Servant.Multipart
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Model.Genre
import WikiMusic.Model.Other
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Free.Backend
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

genresRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> m Html
genresRoute env cookie givenSortOrder limit offset searchInput = do
  maybeGenres <- case searchInput of
    Nothing ->
      liftIO
        $ exec @Backend
          ( getGenres
              env
              (vv ^. #authToken)
              limit'
              offset'
              sortOrder
              (Include {value = "artworks,comments,opinions"})
          )
    Just search ->
      liftIO
        $ exec @Backend
          ( searchGenres
              env
              (vv ^. #authToken)
              search
              limit'
              offset'
              sortOrder
              (Include {value = "artworks,comments,opinions"})
          )

  respondWithViewOrErr
    maybeGenres
    (exec @View . genreListPage limit' offset' env vv)
  where
    vv = vvFromCookies cookie
    sortOrder = maybe (vv ^. #genreSorting) SortOrder givenSortOrder
    limit' = maybe (Limit 25) Limit limit
    offset' = maybe (Offset 0) Offset offset

genreRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
genreRoute env cookie identifier = do
  maybeGenres <-
    liftIO
      $ exec @Backend
        ( getGenre
            env
            (vv ^. #authToken)
            identifier
        )
  respondWithViewOrErr
    maybeGenres
    (exec @View . genreDetailPage env vv)
  where
    vv = vvFromCookies cookie

genreCreateRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
genreCreateRoute env cookie = do
  liftIO $ exec @View (genreCreatePage env vv)
  where
    vv = vvFromCookies cookie

genreCreateFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
genreCreateFormRoute env cookie multipartData = do
  _ <- liftIO $ exec @Backend (createGenre env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Created genre!",
        headers = [withLocation "/genres"]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertGenresRequest
        { genres =
            [ InsertGenresRequestItem
                { displayName = fromForm multipartData "" "displayName",
                  spotifyUrl = maybeFromForm multipartData "spotifyUrl",
                  youtubeUrl = maybeFromForm multipartData "youtubeUrl",
                  soundcloudUrl = maybeFromForm multipartData "soundcloudUrl",
                  wikipediaUrl = maybeFromForm multipartData "wikipediaUrl",
                  description = maybeFromForm multipartData "description"
                }
            ]
        }

genreLikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
genreLikeRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (upsertGenreOpinion env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Liked genre!",
        headers = [withLocation (fromMaybe "/genres" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertGenreOpinionsRequest
        { genreOpinions =
            [ UpsertGenreOpinionsRequestItem
                { genreIdentifier = identifier,
                  isLike = True
                }
            ]
        }

genreDislikeRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
genreDislikeRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (upsertGenreOpinion env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Disliked genre!",
        headers = [withLocation (fromMaybe "/genres" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      UpsertGenreOpinionsRequest
        { genreOpinions =
            [ UpsertGenreOpinionsRequestItem
                { genreIdentifier = identifier,
                  isLike = False
                }
            ]
        }

genreEditRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m Html
genreEditRoute env cookie identifier = do
  maybeGenres <-
    liftIO
      $ exec @Backend
        ( getGenre
            env
            (vv ^. #authToken)
            identifier
        )
  let a = second (\x -> nonEmpty $ mapElems $ x ^. #genres) maybeGenres
  respondWithViewOrErr'
    a
    (exec @View . genreEditPage env vv . head)
  where
    vv = vvFromCookies cookie

genreEditFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
genreEditFormRoute env cookie maybeReferer identifier multipartData = do
  editResult <- liftIO $ exec @Backend (editGenre env (vv ^. #authToken) r)

  respondWithHttp
    httpFound
      { cause = Just "Updated genre!",
        headers = [withLocation (fromMaybe "/genres" maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      GenreDeltaRequest
        { genreDeltas =
            [ GenreDelta
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

searchGenreRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
searchGenreRoute _ multipartData =
  respondWithHttp
    httpFound
      { cause = Just "Go to search genres!",
        headers = [withLocation newRoute]
      }
  where
    searchData = fromForm multipartData "" "searchInput"
    newRoute = "/genres?searchInput=" <> searchData

createGenreArtworkRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
createGenreArtworkRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (createGenreArtwork env (vv ^. #authToken) r)
  respondWithHttp
    httpFound
      { cause = Just "Created genre artwork!",
        headers = [withLocation (maybe "/genres" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      InsertGenreArtworksRequest
        { genreArtworks =
            [ InsertGenreArtworksRequestItem
                { genreIdentifier = identifier,
                  orderValue = fromMaybe 0 $ readMaybe (unpackText . fromMaybe "0" $ maybeFromForm multipartData "orderValue"),
                  contentUrl = fromMaybe "" $ maybeFromForm multipartData "contentUrl",
                  contentCaption = maybeFromForm multipartData "contentCaption"
                }
            ]
        }

genreDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> UUID -> m a
genreDeleteRoute env cookie identifier = do
  _ <- liftIO $ exec @Backend (deleteGenre env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "Deleted genre!",
        headers = [withLocation "/genres"]
      }
  where
    vv = vvFromCookies cookie

genreArtworkDeleteRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> m a
genreArtworkDeleteRoute env cookie maybeReferer identifier = do
  _ <- liftIO $ exec @Backend (deleteGenreArtwork env (vv ^. #authToken) identifier)
  respondWithHttp
    httpFound
      { cause = Just "Deleted genre artwork!",
        headers = [withLocation (maybe "/genres" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie

updateGenreArtworkOrderRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> UUID -> MultipartData tag -> m a
updateGenreArtworkOrderRoute env cookie maybeReferer identifier multipartData = do
  _ <- liftIO $ exec @Backend (updateGenreArtworkOrder env (vv ^. #authToken) r)
  respondWithHttp
    serverResponse
      { code = 302,
        cause = Just "Deleted genre artwork!",
        headers = [withLocation (maybe "/genres" (<> "#edit-artwork") maybeReferer)]
      }
  where
    vv = vvFromCookies cookie
    r =
      GenreArtworkOrderUpdateRequest
        { genreArtworkOrders =
            [ GenreArtworkOrderUpdate
                { identifier = identifier,
                  orderValue = fromMaybe 0 $ readMaybe (unpackText . fromMaybe "0" $ maybeFromForm multipartData "orderValue")
                }
            ]
        }
