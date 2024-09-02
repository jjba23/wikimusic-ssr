module WikiMusic.SSR.Free.Backend
  ( getArtists,
    getArtist,
    Backend (..),
    login,
    getGenres,
    getGenre,
    getSongs,
    getSong,
    searchArtists,
    searchGenres,
    searchSongs,
    createArtist,
    createGenre,
    createSong,
    createSongComment,
    upsertSongOpinion,
    createSongArtwork,
    deleteSong,
    deleteSongComment,
    deleteSongOpinion,
    deleteSongArtwork,
    updateSongArtworkOrder,
    editSong,
    editGenre,
    updateGenreArtworkOrder,
    deleteGenreArtwork,
    deleteGenreOpinion,
    deleteGenreComment,
    deleteGenre,
    createGenreArtwork,
    upsertGenreOpinion,
    createGenreComment,
    createArtistComment,
    upsertArtistOpinion,
    createArtistArtwork,
    deleteArtist,
    deleteArtistComment,
    deleteArtistOpinion,
    editArtist,
    updateArtistArtworkOrder,
    deleteArtistArtwork,
    createArtistOfSong,
    deleteArtistOfSong,
    createSongContents,
    editSongContents,
    deleteSongContents,
    systemInformation,
    resetPassword,
    resetPasswordDo,
    userInvite,
    userDelete,
    me,
  )
where

import Data.UUID (UUID)
import Free.AlaCarte
import Relude
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Interaction.Model.Auth
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Interaction.Model.Song
import WikiMusic.Interaction.Model.User
import WikiMusic.Model.Auth
import WikiMusic.Model.Other
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env

type Backend :: Type -> Type
data Backend a
  = -- artists
    GetArtists Env AuthToken Limit Offset SortOrder Include (Either Text GetArtistsQueryResponse -> a)
  | SearchArtists Env AuthToken Text Limit Offset SortOrder Include (Either Text GetArtistsQueryResponse -> a)
  | GetArtist Env AuthToken UUID (Either Text GetArtistsQueryResponse -> a)
  | CreateArtist Env AuthToken InsertArtistsRequest (Either Text InsertArtistsCommandResponse -> a)
  | CreateArtistComment Env AuthToken InsertArtistCommentsRequest (Either Text InsertArtistCommentsCommandResponse -> a)
  | UpsertArtistOpinion Env AuthToken UpsertArtistOpinionsRequest (Either Text UpsertArtistOpinionsCommandResponse -> a)
  | CreateArtistArtwork Env AuthToken InsertArtistArtworksRequest (Either Text InsertArtistArtworksCommandResponse -> a)
  | DeleteArtist Env AuthToken UUID (Either Text () -> a)
  | DeleteArtistComment Env AuthToken UUID (Either Text () -> a)
  | DeleteArtistOpinion Env AuthToken UUID (Either Text () -> a)
  | DeleteArtistArtwork Env AuthToken UUID (Either Text () -> a)
  | UpdateArtistArtworkOrder Env AuthToken ArtistArtworkOrderUpdateRequest (Either Text () -> a)
  | EditArtist Env AuthToken ArtistDeltaRequest (Either Text () -> a)
  | -- genres
    GetGenres Env AuthToken Limit Offset SortOrder Include (Either Text GetGenresQueryResponse -> a)
  | SearchGenres Env AuthToken Text Limit Offset SortOrder Include (Either Text GetGenresQueryResponse -> a)
  | GetGenre Env AuthToken UUID (Either Text GetGenresQueryResponse -> a)
  | CreateGenre Env AuthToken InsertGenresRequest (Either Text InsertGenresCommandResponse -> a)
  | CreateGenreComment Env AuthToken InsertGenreCommentsRequest (Either Text InsertGenreCommentsCommandResponse -> a)
  | UpsertGenreOpinion Env AuthToken UpsertGenreOpinionsRequest (Either Text UpsertGenreOpinionsCommandResponse -> a)
  | CreateGenreArtwork Env AuthToken InsertGenreArtworksRequest (Either Text InsertGenreArtworksCommandResponse -> a)
  | DeleteGenre Env AuthToken UUID (Either Text () -> a)
  | DeleteGenreComment Env AuthToken UUID (Either Text () -> a)
  | DeleteGenreOpinion Env AuthToken UUID (Either Text () -> a)
  | DeleteGenreArtwork Env AuthToken UUID (Either Text () -> a)
  | UpdateGenreArtworkOrder Env AuthToken GenreArtworkOrderUpdateRequest (Either Text () -> a)
  | EditGenre Env AuthToken GenreDeltaRequest (Either Text () -> a)
  | -- songs
    GetSongs Env AuthToken Limit Offset SortOrder Include (Either Text GetSongsQueryResponse -> a)
  | SearchSongs Env AuthToken Text Limit Offset SortOrder Include (Either Text GetSongsQueryResponse -> a)
  | GetSong Env AuthToken UUID (Either Text GetSongsQueryResponse -> a)
  | CreateSong Env AuthToken InsertSongsRequest (Either Text InsertSongsCommandResponse -> a)
  | CreateSongComment Env AuthToken InsertSongCommentsRequest (Either Text InsertSongCommentsCommandResponse -> a)
  | UpsertSongOpinion Env AuthToken UpsertSongOpinionsRequest (Either Text UpsertSongOpinionsCommandResponse -> a)
  | CreateSongArtwork Env AuthToken InsertSongArtworksRequest (Either Text InsertSongArtworksCommandResponse -> a)
  | CreateArtistOfSong Env AuthToken InsertArtistsOfSongsRequest (Either Text InsertArtistsOfSongCommandResponse -> a)
  | DeleteArtistOfSong Env AuthToken InsertArtistsOfSongsRequest (Either Text () -> a)
  | DeleteSong Env AuthToken UUID (Either Text () -> a)
  | DeleteSongComment Env AuthToken UUID (Either Text () -> a)
  | DeleteSongOpinion Env AuthToken UUID (Either Text () -> a)
  | DeleteSongArtwork Env AuthToken UUID (Either Text () -> a)
  | UpdateSongArtworkOrder Env AuthToken SongArtworkOrderUpdateRequest (Either Text () -> a)
  | EditSong Env AuthToken SongDeltaRequest (Either Text () -> a)
  | CreateSongContents Env AuthToken InsertSongContentsRequest (Either Text InsertSongContentsCommandResponse -> a)
  | DeleteSongContents Env AuthToken UUID (Either Text () -> a)
  | EditSongContents Env AuthToken SongContentDeltaRequest (Either Text () -> a)
  | -- auth
    Me Env AuthToken (Either Text GetMeQueryResponse -> a)
  | UserInvite Env AuthToken InviteUsersRequest (Either Text MakeResetPasswordLinkResponse -> a)
  | UserDelete Env AuthToken DeleteUsersRequest (Either Text () -> a)
  | -- more
    Login Env LoginRequest (Either Text Text -> a)
  | ResetPassword Env Text (Either Text MakeResetPasswordLinkResponse -> a)
  | ResetPasswordDo Env DoPasswordResetRequest (Either Text () -> a)
  | SystemInformation Env (Either Text SystemInformationResponse -> a)
  deriving (Functor)

-- artists

getArtists :: (Backend :<: f) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetArtistsQueryResponse)
getArtists env authToken limit offset sortOrder include = injectFree (GetArtists env authToken limit offset sortOrder include Pure)

searchArtists :: (Backend :<: f) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetArtistsQueryResponse)
searchArtists env authToken search limit offset sortOrder include = injectFree (SearchArtists env authToken search limit offset sortOrder include Pure)

getArtist :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text GetArtistsQueryResponse)
getArtist env authToken identifier = injectFree (GetArtist env authToken identifier Pure)

createArtist :: (Backend :<: f) => Env -> AuthToken -> InsertArtistsRequest -> Free f (Either Text InsertArtistsCommandResponse)
createArtist env authToken r = injectFree (CreateArtist env authToken r Pure)

createArtistComment :: (Backend :<: f) => Env -> AuthToken -> InsertArtistCommentsRequest -> Free f (Either Text InsertArtistCommentsCommandResponse)
createArtistComment env authToken r = injectFree (CreateArtistComment env authToken r Pure)

upsertArtistOpinion :: (Backend :<: f) => Env -> AuthToken -> UpsertArtistOpinionsRequest -> Free f (Either Text UpsertArtistOpinionsCommandResponse)
upsertArtistOpinion env authToken r = injectFree (UpsertArtistOpinion env authToken r Pure)

createArtistArtwork :: (Backend :<: f) => Env -> AuthToken -> InsertArtistArtworksRequest -> Free f (Either Text InsertArtistArtworksCommandResponse)
createArtistArtwork env authToken r = injectFree (CreateArtistArtwork env authToken r Pure)

deleteArtist :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteArtist env authToken identifier = injectFree (DeleteArtist env authToken identifier Pure)

deleteArtistComment :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteArtistComment env authToken identifier = injectFree (DeleteArtistComment env authToken identifier Pure)

deleteArtistOpinion :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteArtistOpinion env authToken identifier = injectFree (DeleteArtistOpinion env authToken identifier Pure)

deleteArtistArtwork :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteArtistArtwork env authToken identifier = injectFree (DeleteArtistArtwork env authToken identifier Pure)

updateArtistArtworkOrder :: (Backend :<: f) => Env -> AuthToken -> ArtistArtworkOrderUpdateRequest -> Free f (Either Text ())
updateArtistArtworkOrder env authToken r = injectFree (UpdateArtistArtworkOrder env authToken r Pure)

editArtist :: (Backend :<: f) => Env -> AuthToken -> ArtistDeltaRequest -> Free f (Either Text ())
editArtist env authToken r = injectFree (EditArtist env authToken r Pure)

-- genres

getGenres :: (Backend :<: f) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetGenresQueryResponse)
getGenres env authToken limit offset sortOrder include = injectFree (GetGenres env authToken limit offset sortOrder include Pure)

searchGenres :: (Backend :<: f) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetGenresQueryResponse)
searchGenres env authToken search limit offset sortOrder include = injectFree (SearchGenres env authToken search limit offset sortOrder include Pure)

getGenre :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text GetGenresQueryResponse)
getGenre env authToken identifier = injectFree (GetGenre env authToken identifier Pure)

createGenre :: (Backend :<: f) => Env -> AuthToken -> InsertGenresRequest -> Free f (Either Text InsertGenresCommandResponse)
createGenre env authToken r = injectFree (CreateGenre env authToken r Pure)

createGenreComment :: (Backend :<: f) => Env -> AuthToken -> InsertGenreCommentsRequest -> Free f (Either Text InsertGenreCommentsCommandResponse)
createGenreComment env authToken r = injectFree (CreateGenreComment env authToken r Pure)

upsertGenreOpinion :: (Backend :<: f) => Env -> AuthToken -> UpsertGenreOpinionsRequest -> Free f (Either Text UpsertGenreOpinionsCommandResponse)
upsertGenreOpinion env authToken r = injectFree (UpsertGenreOpinion env authToken r Pure)

createGenreArtwork :: (Backend :<: f) => Env -> AuthToken -> InsertGenreArtworksRequest -> Free f (Either Text InsertGenreArtworksCommandResponse)
createGenreArtwork env authToken r = injectFree (CreateGenreArtwork env authToken r Pure)

deleteGenre :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteGenre env authToken identifier = injectFree (DeleteGenre env authToken identifier Pure)

deleteGenreComment :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteGenreComment env authToken identifier = injectFree (DeleteGenreComment env authToken identifier Pure)

deleteGenreOpinion :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteGenreOpinion env authToken identifier = injectFree (DeleteGenreOpinion env authToken identifier Pure)

deleteGenreArtwork :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteGenreArtwork env authToken identifier = injectFree (DeleteGenreArtwork env authToken identifier Pure)

updateGenreArtworkOrder :: (Backend :<: f) => Env -> AuthToken -> GenreArtworkOrderUpdateRequest -> Free f (Either Text ())
updateGenreArtworkOrder env authToken r = injectFree (UpdateGenreArtworkOrder env authToken r Pure)

editGenre :: (Backend :<: f) => Env -> AuthToken -> GenreDeltaRequest -> Free f (Either Text ())
editGenre env authToken r = injectFree (EditGenre env authToken r Pure)

-- songs

getSongs :: (Backend :<: f) => Env -> AuthToken -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetSongsQueryResponse)
getSongs env authToken limit offset sortOrder include = injectFree (GetSongs env authToken limit offset sortOrder include Pure)

searchSongs :: (Backend :<: f) => Env -> AuthToken -> Text -> Limit -> Offset -> SortOrder -> Include -> Free f (Either Text GetSongsQueryResponse)
searchSongs env authToken search limit offset sortOrder include = injectFree (SearchSongs env authToken search limit offset sortOrder include Pure)

getSong :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text GetSongsQueryResponse)
getSong env authToken identifier = injectFree (GetSong env authToken identifier Pure)

createSong :: (Backend :<: f) => Env -> AuthToken -> InsertSongsRequest -> Free f (Either Text InsertSongsCommandResponse)
createSong env authToken r = injectFree (CreateSong env authToken r Pure)

createSongComment :: (Backend :<: f) => Env -> AuthToken -> InsertSongCommentsRequest -> Free f (Either Text InsertSongCommentsCommandResponse)
createSongComment env authToken r = injectFree (CreateSongComment env authToken r Pure)

upsertSongOpinion :: (Backend :<: f) => Env -> AuthToken -> UpsertSongOpinionsRequest -> Free f (Either Text UpsertSongOpinionsCommandResponse)
upsertSongOpinion env authToken r = injectFree (UpsertSongOpinion env authToken r Pure)

createSongArtwork :: (Backend :<: f) => Env -> AuthToken -> InsertSongArtworksRequest -> Free f (Either Text InsertSongArtworksCommandResponse)
createSongArtwork env authToken r = injectFree (CreateSongArtwork env authToken r Pure)

createArtistOfSong :: (Backend :<: f) => Env -> AuthToken -> InsertArtistsOfSongsRequest -> Free f (Either Text InsertArtistsOfSongCommandResponse)
createArtistOfSong env authToken r = injectFree (CreateArtistOfSong env authToken r Pure)

deleteArtistOfSong :: (Backend :<: f) => Env -> AuthToken -> InsertArtistsOfSongsRequest -> Free f (Either Text ())
deleteArtistOfSong env authToken r = injectFree (DeleteArtistOfSong env authToken r Pure)

deleteSong :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteSong env authToken identifier = injectFree (DeleteSong env authToken identifier Pure)

deleteSongComment :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteSongComment env authToken identifier = injectFree (DeleteSongComment env authToken identifier Pure)

deleteSongOpinion :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteSongOpinion env authToken identifier = injectFree (DeleteSongOpinion env authToken identifier Pure)

deleteSongArtwork :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteSongArtwork env authToken identifier = injectFree (DeleteSongArtwork env authToken identifier Pure)

updateSongArtworkOrder :: (Backend :<: f) => Env -> AuthToken -> SongArtworkOrderUpdateRequest -> Free f (Either Text ())
updateSongArtworkOrder env authToken r = injectFree (UpdateSongArtworkOrder env authToken r Pure)

editSong :: (Backend :<: f) => Env -> AuthToken -> SongDeltaRequest -> Free f (Either Text ())
editSong env authToken r = injectFree (EditSong env authToken r Pure)

createSongContents :: (Backend :<: f) => Env -> AuthToken -> InsertSongContentsRequest -> Free f (Either Text InsertSongContentsCommandResponse)
createSongContents env authToken r = injectFree (CreateSongContents env authToken r Pure)

deleteSongContents :: (Backend :<: f) => Env -> AuthToken -> UUID -> Free f (Either Text ())
deleteSongContents env authToken identifier = injectFree (DeleteSongContents env authToken identifier Pure)

editSongContents :: (Backend :<: f) => Env -> AuthToken -> SongContentDeltaRequest -> Free f (Either Text ())
editSongContents env authToken r = injectFree (EditSongContents env authToken r Pure)

me :: (Backend :<: f) => Env -> AuthToken -> Free f (Either Text GetMeQueryResponse)
me env authToken = injectFree (Me env authToken Pure)

userInvite :: (Backend :<: f) => Env -> AuthToken -> InviteUsersRequest -> Free f (Either Text MakeResetPasswordLinkResponse)
userInvite env authToken r = injectFree (UserInvite env authToken r Pure)

userDelete :: (Backend :<: f) => Env -> AuthToken -> DeleteUsersRequest -> Free f (Either Text ())
userDelete env authToken r = injectFree (UserDelete env authToken r Pure)

-- more
login :: (Backend :<: f) => Env -> LoginRequest -> Free f (Either Text Text)
login env loginRequest = injectFree (Login env loginRequest Pure)

resetPassword :: (Backend :<: f) => Env -> Text -> Free f (Either Text MakeResetPasswordLinkResponse)
resetPassword env r = injectFree (ResetPassword env r Pure)

resetPasswordDo :: (Backend :<: f) => Env -> DoPasswordResetRequest -> Free f (Either Text ())
resetPasswordDo env r = injectFree (ResetPasswordDo env r Pure)

systemInformation :: (Backend :<: f) => Env -> Free f (Either Text SystemInformationResponse)
systemInformation env = injectFree (SystemInformation env Pure)
