module WikiMusic.SSR.Free.View where

import Principium
import WikiMusic.Interaction.Model.Artist
import WikiMusic.Interaction.Model.Genre
import WikiMusic.Interaction.Model.Song

type View :: Type -> Type
data View a
  = ArtistListPage Limit Offset Env ViewVars GetArtistsQueryResponse (Html -> a)
  | ArtistDetailPage Env ViewVars GetArtistsQueryResponse (Html -> a)
  | ArtistCreatePage Env ViewVars (Html -> a)
  | ArtistEditPage Env ViewVars Artist (Html -> a)
  | GenreListPage Limit Offset Env ViewVars GetGenresQueryResponse (Html -> a)
  | GenreDetailPage Env ViewVars GetGenresQueryResponse (Html -> a)
  | GenreCreatePage Env ViewVars (Html -> a)
  | GenreEditPage Env ViewVars Genre (Html -> a)
  | SongListPage Limit Offset Env ViewVars GetSongsQueryResponse (Html -> a)
  | SongDetailPage Env ViewVars GetSongsQueryResponse (Html -> a)
  | SongCreatePage Env ViewVars (Html -> a)
  | SongEditPage Env ViewVars Song (Html -> a)
  | ErrorPage Env ViewVars (Maybe Int) (Maybe Text) (Html -> a)
  | LoginPage Env ViewVars (Html -> a)
  | RequestPasswordResetPage Env ViewVars (Html -> a)
  | DoPasswordResetPage Env ViewVars (Maybe Text) (Html -> a)
  | InviteUsersPage Env ViewVars (Html -> a)
  deriving (Functor)

artistListPage :: (View :<: f) => Limit -> Offset -> Env -> ViewVars -> GetArtistsQueryResponse -> Free f Html
artistListPage limit offset env vv artists = injectFree (ArtistListPage limit offset env vv artists Pure)

artistDetailPage :: (View :<: f) => Env -> ViewVars -> GetArtistsQueryResponse -> Free f Html
artistDetailPage env vv artists = injectFree (ArtistDetailPage env vv artists Pure)

artistCreatePage :: (View :<: f) => Env -> ViewVars -> Free f Html
artistCreatePage env vv = injectFree (ArtistCreatePage env vv Pure)

artistEditPage :: (View :<: f) => Env -> ViewVars -> Artist -> Free f Html
artistEditPage env vv artist = injectFree (ArtistEditPage env vv artist Pure)

genreListPage :: (View :<: f) => Limit -> Offset -> Env -> ViewVars -> GetGenresQueryResponse -> Free f Html
genreListPage limit offset env vv genres = injectFree (GenreListPage limit offset env vv genres Pure)

genreDetailPage :: (View :<: f) => Env -> ViewVars -> GetGenresQueryResponse -> Free f Html
genreDetailPage env vv genres = injectFree (GenreDetailPage env vv genres Pure)

genreCreatePage :: (View :<: f) => Env -> ViewVars -> Free f Html
genreCreatePage env vv = injectFree (GenreCreatePage env vv Pure)

genreEditPage :: (View :<: f) => Env -> ViewVars -> Genre -> Free f Html
genreEditPage env vv genre = injectFree (GenreEditPage env vv genre Pure)

songListPage :: (View :<: f) => Limit -> Offset -> Env -> ViewVars -> GetSongsQueryResponse -> Free f Html
songListPage limit offset env vv songs = injectFree (SongListPage limit offset env vv songs Pure)

songDetailPage :: (View :<: f) => Env -> ViewVars -> GetSongsQueryResponse -> Free f Html
songDetailPage env vv songs = injectFree (SongDetailPage env vv songs Pure)

songCreatePage :: (View :<: f) => Env -> ViewVars -> Free f Html
songCreatePage env vv = injectFree (SongCreatePage env vv Pure)

songEditPage :: (View :<: f) => Env -> ViewVars -> Song -> Free f Html
songEditPage env vv song = injectFree (SongEditPage env vv song Pure)

errorPage :: (View :<: f) => Env -> ViewVars -> Maybe Int -> Maybe Text -> Free f Html
errorPage env vv maybeCode maybeMessage = injectFree (ErrorPage env vv maybeCode maybeMessage Pure)

loginPage :: (View :<: f) => Env -> ViewVars -> Free f Html
loginPage env vv = injectFree (LoginPage env vv Pure)

requestPasswordResetPage :: (View :<: f) => Env -> ViewVars -> Free f Html
requestPasswordResetPage env vv = injectFree (RequestPasswordResetPage env vv Pure)

doPasswordResetPage :: (View :<: f) => Env -> ViewVars -> Maybe Text -> Free f Html
doPasswordResetPage env vv t = injectFree (DoPasswordResetPage env vv t Pure)

inviteUsersPage :: (View :<: f) => Env -> ViewVars -> Free f Html
inviteUsersPage env vv = injectFree (InviteUsersPage env vv Pure)
