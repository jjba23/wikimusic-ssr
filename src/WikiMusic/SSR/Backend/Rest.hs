{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.Backend.Rest () where

import Data.Text (pack)
import Data.UUID (UUID)
import Free.AlaCarte
import Optics
import Relude
import Servant
import Servant.Client
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Interaction.Model.Auth
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Interaction.Model.Song
import WikiMusic.Interaction.Model.User
import WikiMusic.Model.Auth
import WikiMusic.Model.Other
import WikiMusic.SSR.Free.Backend
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.Servant.ApiSpec

instance Exec Backend where
  -- artists
  execAlgebra (GetArtists env authToken limit offset sortOrder include next) =
    next =<< getArtists' env authToken limit offset sortOrder include
  execAlgebra (SearchArtists env authToken search limit offset sortOrder include next) =
    next =<< searchArtists' env authToken search limit offset sortOrder include
  execAlgebra (GetArtist env authToken identifier next) =
    next =<< getArtist' env authToken identifier
  execAlgebra (CreateArtist env authToken r next) =
    next =<< createArtist' env authToken r
  execAlgebra (CreateArtistComment env authToken r next) =
    next =<< createArtistComment' env authToken r
  execAlgebra (UpsertArtistOpinion env authToken r next) =
    next =<< upsertArtistOpinion' env authToken r
  execAlgebra (CreateArtistArtwork env authToken r next) =
    next =<< createArtistArtwork' env authToken r
  execAlgebra (DeleteArtist env authToken identifier next) =
    next =<< deleteArtist' env authToken identifier
  execAlgebra (DeleteArtistComment env authToken identifier next) =
    next =<< deleteArtistComment' env authToken identifier
  execAlgebra (DeleteArtistOpinion env authToken identifier next) =
    next =<< deleteArtistOpinion' env authToken identifier
  execAlgebra (DeleteArtistArtwork env authToken identifier next) =
    next =<< deleteArtistArtwork' env authToken identifier
  execAlgebra (UpdateArtistArtworkOrder env authToken r next) =
    next =<< updateArtistArtworkOrder' env authToken r
  execAlgebra (EditArtist env authToken r next) =
    next =<< editArtist' env authToken r
  -- genres
  execAlgebra (GetGenres env authToken limit offset sortOrder include next) =
    next =<< getGenres' env authToken limit offset sortOrder include
  execAlgebra (SearchGenres env authToken search limit offset sortOrder include next) =
    next =<< searchGenres' env authToken search limit offset sortOrder include
  execAlgebra (GetGenre env authToken identifier next) =
    next =<< getGenre' env authToken identifier
  execAlgebra (CreateGenre env authToken r next) =
    next =<< createGenre' env authToken r
  execAlgebra (CreateGenreComment env authToken r next) =
    next =<< createGenreComment' env authToken r
  execAlgebra (UpsertGenreOpinion env authToken r next) =
    next =<< upsertGenreOpinion' env authToken r
  execAlgebra (CreateGenreArtwork env authToken r next) =
    next =<< createGenreArtwork' env authToken r
  execAlgebra (DeleteGenre env authToken identifier next) =
    next =<< deleteGenre' env authToken identifier
  execAlgebra (DeleteGenreComment env authToken identifier next) =
    next =<< deleteGenreComment' env authToken identifier
  execAlgebra (DeleteGenreOpinion env authToken identifier next) =
    next =<< deleteGenreOpinion' env authToken identifier
  execAlgebra (DeleteGenreArtwork env authToken identifier next) =
    next =<< deleteGenreArtwork' env authToken identifier
  execAlgebra (UpdateGenreArtworkOrder env authToken r next) =
    next =<< updateGenreArtworkOrder' env authToken r
  execAlgebra (EditGenre env authToken r next) =
    next =<< editGenre' env authToken r
  -- songs
  execAlgebra (GetSongs env authToken limit offset sortOrder include next) =
    next =<< getSongs' env authToken limit offset sortOrder include
  execAlgebra (SearchSongs env authToken search limit offset sortOrder include next) =
    next =<< searchSongs' env authToken search limit offset sortOrder include
  execAlgebra (GetSong env authToken identifier next) =
    next =<< getSong' env authToken identifier
  execAlgebra (CreateSong env authToken r next) =
    next =<< createSong' env authToken r
  execAlgebra (CreateSongComment env authToken r next) =
    next =<< createSongComment' env authToken r
  execAlgebra (UpsertSongOpinion env authToken r next) =
    next =<< upsertSongOpinion' env authToken r
  execAlgebra (CreateSongArtwork env authToken r next) =
    next =<< createSongArtwork' env authToken r
  execAlgebra (CreateArtistOfSong env authToken r next) =
    next =<< createArtistOfSong' env authToken r
  execAlgebra (DeleteArtistOfSong env authToken r next) =
    next =<< deleteArtistOfSong' env authToken r
  execAlgebra (DeleteSong env authToken identifier next) =
    next =<< deleteSong' env authToken identifier
  execAlgebra (DeleteSongComment env authToken identifier next) =
    next =<< deleteSongComment' env authToken identifier
  execAlgebra (DeleteSongOpinion env authToken identifier next) =
    next =<< deleteSongOpinion' env authToken identifier
  execAlgebra (DeleteSongArtwork env authToken identifier next) =
    next =<< deleteSongArtwork' env authToken identifier
  execAlgebra (UpdateSongArtworkOrder env authToken r next) =
    next =<< updateSongArtworkOrder' env authToken r
  execAlgebra (EditSong env authToken r next) =
    next =<< editSong' env authToken r
  execAlgebra (CreateSongContents env authToken r next) =
    next =<< createSongContents' env authToken r
  execAlgebra (DeleteSongContents env authToken identifier next) =
    next =<< deleteSongContents' env authToken identifier
  execAlgebra (EditSongContents env authToken r next) =
    next =<< editSongContents' env authToken r
  -- auth
  execAlgebra (Me env authToken next) =
    next =<< me' env authToken
  execAlgebra (UserInvite env authToken r next) =
    next =<< userInvite' env authToken r
  execAlgebra (UserDelete env authToken r next) =
    next =<< userDelete' env authToken r
  -- more
  execAlgebra (Login env loginRequest next) =
    next =<< login' env loginRequest
  execAlgebra (ResetPassword env r next) =
    next =<< resetPassword' env r
  execAlgebra (ResetPasswordDo env r next) =
    next =<< resetPasswordDo' env r
  execAlgebra (SystemInformation env next) =
    next =<< systemInformation' env

-- artists

getArtists' :: (MonadIO m) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetArtistsQueryResponse)
getArtists' env authToken (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ getArtistsHTTP (Just $ authToken ^. #value) (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

searchArtists' :: (MonadIO m) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetArtistsQueryResponse)
searchArtists' env authToken search (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ searchArtistsHTTP (Just $ authToken ^. #value) search (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

getArtist' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text GetArtistsQueryResponse)
getArtist' env authToken identifier = do
  doRest env
    $ getArtistHTTP (Just $ authToken ^. #value) identifier

createArtist' :: (MonadIO m) => Env -> AuthToken -> InsertArtistsRequest -> m (Either Text InsertArtistsCommandResponse)
createArtist' env authToken r = do
  doRest env
    $ createArtistHTTP (Just $ authToken ^. #value) r

createArtistComment' :: (MonadIO m) => Env -> AuthToken -> InsertArtistCommentsRequest -> m (Either Text InsertArtistCommentsCommandResponse)
createArtistComment' env authToken r = do
  doRest env
    $ createArtistCommentHTTP (Just $ authToken ^. #value) r

upsertArtistOpinion' :: (MonadIO m) => Env -> AuthToken -> UpsertArtistOpinionsRequest -> m (Either Text UpsertArtistOpinionsCommandResponse)
upsertArtistOpinion' env authToken r = do
  doRest env
    $ upsertArtistOpinionHTTP (Just $ authToken ^. #value) r

createArtistArtwork' :: (MonadIO m) => Env -> AuthToken -> InsertArtistArtworksRequest -> m (Either Text InsertArtistArtworksCommandResponse)
createArtistArtwork' env authToken r = do
  doRest env
    $ createArtistArtworkHTTP (Just $ authToken ^. #value) r

deleteArtist' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteArtist' env authToken identifier = do
  doRest env
    $ deleteArtistHTTP (Just $ authToken ^. #value) identifier

deleteArtistComment' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteArtistComment' env authToken identifier = do
  doRest env
    $ deleteArtistCommentHTTP (Just $ authToken ^. #value) identifier

deleteArtistOpinion' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteArtistOpinion' env authToken identifier = do
  doRest env
    $ deleteArtistOpinionHTTP (Just $ authToken ^. #value) identifier

deleteArtistArtwork' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteArtistArtwork' env authToken identifier = do
  doRest env
    $ deleteArtistArtworkHTTP (Just $ authToken ^. #value) identifier

updateArtistArtworkOrder' :: (MonadIO m) => Env -> AuthToken -> ArtistArtworkOrderUpdateRequest -> m (Either Text ())
updateArtistArtworkOrder' env authToken r = do
  doRest env
    $ updateArtistArtworkOrderHTTP (Just $ authToken ^. #value) r

editArtist' :: (MonadIO m) => Env -> AuthToken -> ArtistDeltaRequest -> m (Either Text ())
editArtist' env authToken r = do
  doRest env
    $ editArtistHTTP (Just $ authToken ^. #value) r

-- genres

getGenres' :: (MonadIO m) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetGenresQueryResponse)
getGenres' env authToken (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ getGenresHTTP (Just $ authToken ^. #value) (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

searchGenres' :: (MonadIO m) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetGenresQueryResponse)
searchGenres' env authToken search (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ searchGenresHTTP (Just $ authToken ^. #value) search (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

getGenre' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text GetGenresQueryResponse)
getGenre' env authToken identifier = do
  doRest env
    $ getGenreHTTP (Just $ authToken ^. #value) identifier

createGenre' :: (MonadIO m) => Env -> AuthToken -> InsertGenresRequest -> m (Either Text InsertGenresCommandResponse)
createGenre' env authToken r = do
  doRest env
    $ createGenreHTTP (Just $ authToken ^. #value) r

createGenreComment' :: (MonadIO m) => Env -> AuthToken -> InsertGenreCommentsRequest -> m (Either Text InsertGenreCommentsCommandResponse)
createGenreComment' env authToken r = do
  doRest env
    $ createGenreCommentHTTP (Just $ authToken ^. #value) r

upsertGenreOpinion' :: (MonadIO m) => Env -> AuthToken -> UpsertGenreOpinionsRequest -> m (Either Text UpsertGenreOpinionsCommandResponse)
upsertGenreOpinion' env authToken r = do
  doRest env
    $ upsertGenreOpinionHTTP (Just $ authToken ^. #value) r

createGenreArtwork' :: (MonadIO m) => Env -> AuthToken -> InsertGenreArtworksRequest -> m (Either Text InsertGenreArtworksCommandResponse)
createGenreArtwork' env authToken r = do
  doRest env
    $ createGenreArtworkHTTP (Just $ authToken ^. #value) r

deleteGenre' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteGenre' env authToken identifier = do
  doRest env
    $ deleteGenreHTTP (Just $ authToken ^. #value) identifier

deleteGenreComment' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteGenreComment' env authToken identifier = do
  doRest env
    $ deleteGenreCommentHTTP (Just $ authToken ^. #value) identifier

deleteGenreOpinion' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteGenreOpinion' env authToken identifier = do
  doRest env
    $ deleteGenreOpinionHTTP (Just $ authToken ^. #value) identifier

deleteGenreArtwork' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteGenreArtwork' env authToken identifier = do
  doRest env
    $ deleteGenreArtworkHTTP (Just $ authToken ^. #value) identifier

updateGenreArtworkOrder' :: (MonadIO m) => Env -> AuthToken -> GenreArtworkOrderUpdateRequest -> m (Either Text ())
updateGenreArtworkOrder' env authToken r = do
  doRest env
    $ updateGenreArtworkOrderHTTP (Just $ authToken ^. #value) r

editGenre' :: (MonadIO m) => Env -> AuthToken -> GenreDeltaRequest -> m (Either Text ())
editGenre' env authToken r = do
  doRest env
    $ editGenreHTTP (Just $ authToken ^. #value) r

-- songs

getSongs' :: (MonadIO m) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetSongsQueryResponse)
getSongs' env authToken (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ getSongsHTTP (Just $ authToken ^. #value) (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

searchSongs' :: (MonadIO m) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> m (Either Text GetSongsQueryResponse)
searchSongs' env authToken search (Limit limit) (Offset offset) sortOrder include = do
  doRest env
    $ searchSongsHTTP (Just $ authToken ^. #value) search (Just limit) (Just offset) (Just $ sortOrder ^. #value) (Just $ include ^. #value)

getSong' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text GetSongsQueryResponse)
getSong' env authToken identifier = do
  doRest env
    $ getSongHTTP (Just $ authToken ^. #value) identifier

createSong' :: (MonadIO m) => Env -> AuthToken -> InsertSongsRequest -> m (Either Text InsertSongsCommandResponse)
createSong' env authToken r = do
  doRest env
    $ createSongHTTP (Just $ authToken ^. #value) r

createSongComment' :: (MonadIO m) => Env -> AuthToken -> InsertSongCommentsRequest -> m (Either Text InsertSongCommentsCommandResponse)
createSongComment' env authToken r = do
  doRest env
    $ createSongCommentHTTP (Just $ authToken ^. #value) r

upsertSongOpinion' :: (MonadIO m) => Env -> AuthToken -> UpsertSongOpinionsRequest -> m (Either Text UpsertSongOpinionsCommandResponse)
upsertSongOpinion' env authToken r = do
  doRest env
    $ upsertSongOpinionHTTP (Just $ authToken ^. #value) r

createSongArtwork' :: (MonadIO m) => Env -> AuthToken -> InsertSongArtworksRequest -> m (Either Text InsertSongArtworksCommandResponse)
createSongArtwork' env authToken r = do
  doRest env
    $ createSongArtworkHTTP (Just $ authToken ^. #value) r

deleteSong' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteSong' env authToken identifier = do
  doRest env
    $ deleteSongHTTP (Just $ authToken ^. #value) identifier

deleteSongComment' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteSongComment' env authToken identifier = do
  doRest env
    $ deleteSongCommentHTTP (Just $ authToken ^. #value) identifier

deleteSongOpinion' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteSongOpinion' env authToken identifier = do
  doRest env
    $ deleteSongOpinionHTTP (Just $ authToken ^. #value) identifier

deleteSongArtwork' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteSongArtwork' env authToken identifier = do
  doRest env
    $ deleteSongArtworkHTTP (Just $ authToken ^. #value) identifier

updateSongArtworkOrder' :: (MonadIO m) => Env -> AuthToken -> SongArtworkOrderUpdateRequest -> m (Either Text ())
updateSongArtworkOrder' env authToken r = do
  doRest env
    $ updateSongArtworkOrderHTTP (Just $ authToken ^. #value) r

editSong' :: (MonadIO m) => Env -> AuthToken -> SongDeltaRequest -> m (Either Text ())
editSong' env authToken r = do
  doRest env
    $ editSongHTTP (Just $ authToken ^. #value) r

createArtistOfSong' :: (MonadIO m) => Env -> AuthToken -> InsertArtistsOfSongsRequest -> m (Either Text InsertArtistsOfSongCommandResponse)
createArtistOfSong' env authToken r = do
  doRest env
    $ createArtistOfSongHTTP (Just $ authToken ^. #value) r

deleteArtistOfSong' :: (MonadIO m) => Env -> AuthToken -> InsertArtistsOfSongsRequest -> m (Either Text ())
deleteArtistOfSong' env authToken r = do
  doRest env
    $ deleteArtistOfSongHTTP (Just $ authToken ^. #value) r

createSongContents' :: (MonadIO m) => Env -> AuthToken -> InsertSongContentsRequest -> m (Either Text InsertSongContentsCommandResponse)
createSongContents' env authToken r = do
  doRest env
    $ createSongContentsHTTP (Just $ authToken ^. #value) r

deleteSongContents' :: (MonadIO m) => Env -> AuthToken -> UUID -> m (Either Text ())
deleteSongContents' env authToken identifier = do
  doRest env
    $ deleteSongContentsHTTP (Just $ authToken ^. #value) identifier

editSongContents' :: (MonadIO m) => Env -> AuthToken -> SongContentDeltaRequest -> m (Either Text ())
editSongContents' env authToken r = do
  doRest env
    $ editSongContentsHTTP (Just $ authToken ^. #value) r

-- auth
me' :: (MonadIO m) => Env -> AuthToken -> m (Either Text GetMeQueryResponse)
me' env authToken = doRest env $ meHTTP (Just $ authToken ^. #value)

userInvite' :: (MonadIO m) => Env -> AuthToken -> InviteUsersRequest -> m (Either Text MakeResetPasswordLinkResponse)
userInvite' env authToken r = doRest env $ userInviteHTTP (Just $ authToken ^. #value) r

userDelete' :: (MonadIO m) => Env -> AuthToken -> DeleteUsersRequest -> m (Either Text ())
userDelete' env authToken r = doRest env $ userDeleteHTTP (Just $ authToken ^. #value) r

-- more

login' :: (MonadIO m) => Env -> LoginRequest -> m (Either Text Text)
login' env loginRequest = do
  eitherResponse <- doRest env $ loginHTTP loginRequest
  let maybeParsedHeaders = second (nonEmpty . map snd . filter (\(name, _) -> name == "x-wikimusic-auth") . getHeaders) eitherResponse
  case maybeParsedHeaders of
    Left e -> pure . Left $ e
    Right Nothing -> pure . Left $ "No x-wikimusic-auth header was found!"
    Right (Just h) -> pure . Right . decodeUtf8 . head $ h

resetPassword' :: (MonadIO m) => Env -> Text -> m (Either Text MakeResetPasswordLinkResponse)
resetPassword' env r = doRest env $ resetPasswordHTTP r

resetPasswordDo' :: (MonadIO m) => Env -> DoPasswordResetRequest -> m (Either Text ())
resetPasswordDo' env r = doRest env $ resetPasswordDoHTTP r

systemInformation' :: (MonadIO m) => Env -> m (Either Text SystemInformationResponse)
systemInformation' env = doRest env systemInformationHTTP

--
--
--

doRest :: (MonadIO m) => Env -> ClientM r -> m (Either Text r)
doRest env restCall = do
  eff <- liftIO $ runClientM restCall (env ^. #clientEnv)
  pure $ first (pack . Relude.show) eff

backendAPI :: Proxy BackendAPI
backendAPI = Proxy

type BackendAPI =
  ArtistsAPI
    :<|> GenresAPI
    :<|> SongsAPI
    :<|> AuthAPI
    :<|> PublicAPI

--
getArtistsHTTP :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetArtistsQueryResponse
searchArtistsHTTP :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetArtistsQueryResponse
getArtistHTTP :: Maybe Text -> UUID -> ClientM GetArtistsQueryResponse
createArtistHTTP :: Maybe Text -> InsertArtistsRequest -> ClientM InsertArtistsCommandResponse
createArtistCommentHTTP :: Maybe Text -> InsertArtistCommentsRequest -> ClientM InsertArtistCommentsCommandResponse
upsertArtistOpinionHTTP :: Maybe Text -> UpsertArtistOpinionsRequest -> ClientM UpsertArtistOpinionsCommandResponse
createArtistArtworkHTTP :: Maybe Text -> InsertArtistArtworksRequest -> ClientM InsertArtistArtworksCommandResponse
deleteArtistHTTP :: Maybe Text -> UUID -> ClientM ()
deleteArtistCommentHTTP :: Maybe Text -> UUID -> ClientM ()
deleteArtistOpinionHTTP :: Maybe Text -> UUID -> ClientM ()
deleteArtistArtworkHTTP :: Maybe Text -> UUID -> ClientM ()
updateArtistArtworkOrderHTTP :: Maybe Text -> ArtistArtworkOrderUpdateRequest -> ClientM ()
editArtistHTTP :: Maybe Text -> ArtistDeltaRequest -> ClientM ()
--
getGenresHTTP :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetGenresQueryResponse
searchGenresHTTP :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetGenresQueryResponse
getGenreHTTP :: Maybe Text -> UUID -> ClientM GetGenresQueryResponse
createGenreHTTP :: Maybe Text -> InsertGenresRequest -> ClientM InsertGenresCommandResponse
createGenreCommentHTTP :: Maybe Text -> InsertGenreCommentsRequest -> ClientM InsertGenreCommentsCommandResponse
upsertGenreOpinionHTTP :: Maybe Text -> UpsertGenreOpinionsRequest -> ClientM UpsertGenreOpinionsCommandResponse
createGenreArtworkHTTP :: Maybe Text -> InsertGenreArtworksRequest -> ClientM InsertGenreArtworksCommandResponse
deleteGenreHTTP :: Maybe Text -> UUID -> ClientM ()
deleteGenreCommentHTTP :: Maybe Text -> UUID -> ClientM ()
deleteGenreOpinionHTTP :: Maybe Text -> UUID -> ClientM ()
deleteGenreArtworkHTTP :: Maybe Text -> UUID -> ClientM ()
updateGenreArtworkOrderHTTP :: Maybe Text -> GenreArtworkOrderUpdateRequest -> ClientM ()
editGenreHTTP :: Maybe Text -> GenreDeltaRequest -> ClientM ()
--
getSongsHTTP :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetSongsQueryResponse
searchSongsHTTP :: Maybe Text -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ClientM GetSongsQueryResponse
getSongHTTP :: Maybe Text -> UUID -> ClientM GetSongsQueryResponse
createSongHTTP :: Maybe Text -> InsertSongsRequest -> ClientM InsertSongsCommandResponse
createSongCommentHTTP :: Maybe Text -> InsertSongCommentsRequest -> ClientM InsertSongCommentsCommandResponse
upsertSongOpinionHTTP :: Maybe Text -> UpsertSongOpinionsRequest -> ClientM UpsertSongOpinionsCommandResponse
createSongArtworkHTTP :: Maybe Text -> InsertSongArtworksRequest -> ClientM InsertSongArtworksCommandResponse
createArtistOfSongHTTP :: Maybe Text -> InsertArtistsOfSongsRequest -> ClientM InsertArtistsOfSongCommandResponse
deleteArtistOfSongHTTP :: Maybe Text -> InsertArtistsOfSongsRequest -> ClientM ()
deleteSongHTTP :: Maybe Text -> UUID -> ClientM ()
deleteSongCommentHTTP :: Maybe Text -> UUID -> ClientM ()
deleteSongOpinionHTTP :: Maybe Text -> UUID -> ClientM ()
deleteSongArtworkHTTP :: Maybe Text -> UUID -> ClientM ()
updateSongArtworkOrderHTTP :: Maybe Text -> SongArtworkOrderUpdateRequest -> ClientM ()
editSongHTTP :: Maybe Text -> SongDeltaRequest -> ClientM ()
createSongContentsHTTP :: Maybe Text -> InsertSongContentsRequest -> ClientM InsertSongContentsCommandResponse
deleteSongContentsHTTP :: Maybe Text -> UUID -> ClientM ()
editSongContentsHTTP :: Maybe Text -> SongContentDeltaRequest -> ClientM ()
--
meHTTP :: Maybe Text -> ClientM GetMeQueryResponse
userInviteHTTP :: Maybe Text -> InviteUsersRequest -> ClientM MakeResetPasswordLinkResponse
userDeleteHTTP :: Maybe Text -> DeleteUsersRequest -> ClientM ()
--
loginHTTP ::
  LoginRequest ->
  ClientM
    ( Headers
        '[Header "x-wikimusic-auth" Text]
        NoContent
    )
resetPasswordHTTP :: Text -> ClientM MakeResetPasswordLinkResponse
resetPasswordDoHTTP :: DoPasswordResetRequest -> ClientM ()
systemInformationHTTP :: ClientM SystemInformationResponse
-- artists
( ( deleteArtistHTTP
      :<|> deleteArtistCommentHTTP
      :<|> deleteArtistOpinionHTTP
      :<|> deleteArtistArtworkHTTP
    )
    :<|> getArtistsHTTP
    :<|> searchArtistsHTTP
    :<|> getArtistHTTP
    :<|> createArtistHTTP
    :<|> createArtistCommentHTTP
    :<|> upsertArtistOpinionHTTP
    :<|> createArtistArtworkHTTP
    :<|> updateArtistArtworkOrderHTTP
    :<|> editArtistHTTP
  )
  -- genres
  :<|> ( ( deleteGenreHTTP
             :<|> deleteGenreCommentHTTP
             :<|> deleteGenreOpinionHTTP
             :<|> deleteGenreArtworkHTTP
           )
           :<|> getGenresHTTP
           :<|> searchGenresHTTP
           :<|> getGenreHTTP
           :<|> createGenreHTTP
           :<|> createGenreCommentHTTP
           :<|> upsertGenreOpinionHTTP
           :<|> createGenreArtworkHTTP
           :<|> updateGenreArtworkOrderHTTP
           :<|> editGenreHTTP
         )
  -- songs
  :<|> ( ( deleteSongHTTP
             :<|> deleteSongCommentHTTP
             :<|> deleteSongOpinionHTTP
             :<|> deleteSongArtworkHTTP
           )
           :<|> getSongsHTTP
           :<|> searchSongsHTTP
           :<|> getSongHTTP
           :<|> createSongHTTP
           :<|> createSongCommentHTTP
           :<|> upsertSongOpinionHTTP
           :<|> createSongArtworkHTTP
           :<|> createArtistOfSongHTTP
           :<|> deleteArtistOfSongHTTP
           :<|> updateSongArtworkOrderHTTP
           :<|> editSongHTTP
           :<|> createSongContentsHTTP
           :<|> deleteSongContentsHTTP
           :<|> editSongContentsHTTP
         )
  -- auth
  :<|> (meHTTP :<|> (userInviteHTTP :<|> userDeleteHTTP))
  -- public
  :<|> (loginHTTP :<|> (resetPasswordHTTP :<|> resetPasswordDoHTTP) :<|> systemInformationHTTP) =
    client backendAPI
