module WikiMusic.SSR.Servant.ApiSpec where

import Data.UUID (UUID)
import Relude
import Servant
import Servant.HTML.Blaze as ServantBlaze
import Servant.Multipart
import Text.Blaze.Html as Html
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.View.Html ()

type WikiMusicSSRServant =
  Get '[ServantBlaze.HTML] NoContent
    :<|> "artists"
      :> BaseEntityRoutes
    :<|> "genres"
      :> BaseEntityRoutes
    :<|> "songs"
      :> ( BaseEntityRoutes
             :<|> SongContentsRoutes
         )
    :<|> "user-preferences"
      :> PreferenceRoutes
    :<|> "login" :> LoginRoutes
    :<|> "error" :> ErrorRoute
    :<|> "passwords" :> PasswordRoutes
    :<|> "users" :> UserRoutes

type PasswordRoutes =
  "request-reset" :> WithCookie :> HtmlDoc
    :<|> "request-reset" :> PostDataForm
    :<|> "do-reset" :> WithCookie :> QueryParam "token" Text :> HtmlDoc
    :<|> "do-reset" :> PostDataForm

type UserRoutes =
  "invite" :> WithCookie :> HtmlDoc
    :<|> "invite" :> WithCookie :> PostDataForm

type PostDataForm =
  MultipartForm Mem (MultipartData Mem)
    :> Post '[ServantBlaze.HTML] (Headers '[Header "Location" Text] NoContent)

type PostNoFields = Post '[ServantBlaze.HTML] (Headers '[Header "Location" Text] NoContent)

type WithReferer = Header "referer" Text

type CookieForm =
  WithReferer
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[ServantBlaze.HTML] (Headers '[Header "Set-Cookie" Text, Header "Location" Text] NoContent)

type HtmlDoc = Get '[ServantBlaze.HTML] Html

type WithCookie = Header "cookie" Text

type BaseEntityRoutes =
  WithCookie
    :> QueryParam "sort-order" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> QueryParam "searchInput" Text
    :> HtmlDoc
    :<|> WithCookie :> WithEntityIdentifier :> HtmlDoc
    :<|> "create" :> WithCookie :> HtmlDoc
    :<|> "create" :> WithCookie :> PostDataForm
    :<|> "like" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostNoFields
    :<|> "dislike" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostNoFields
    :<|> "edit" :> WithCookie :> WithEntityIdentifier :> HtmlDoc
    :<|> "edit" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostDataForm
    :<|> "search" :> PostDataForm
    :<|> "artworks"
      :> ( "create" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostDataForm
             :<|> "delete" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostNoFields
             :<|> "order" :> WithCookie :> WithReferer :> WithEntityIdentifier :> PostDataForm
         )
    :<|> "delete" :> WithCookie :> WithEntityIdentifier :> PostNoFields

type SongContentsRoutes =
  WithCookie
    :> WithReferer
    :> WithEntityIdentifier
    :> "contents"
    :> PostDataForm
    :<|> WithCookie
      :> WithReferer
      :> WithEntityIdentifier
      :> "contents"
      :> Capture "songContentIdentifier" UUID
      :> PostDataForm
    :<|> "contents" :> WithEntityIdentifier :> "delete" :> WithCookie :> WithReferer :> PostNoFields

type PreferenceRoutes =
  "locale" :> CookieForm
    :<|> "artist-sorting" :> CookieForm
    :<|> "genre-sorting" :> CookieForm
    :<|> "song-sorting" :> CookieForm
    :<|> "dark-mode" :> CookieForm
    :<|> "song-ascii-size" :> CookieForm
    :<|> "palette" :> CookieForm

type LoginRoutes = WithCookie :> HtmlDoc :<|> PostDataForm

type WithEntityIdentifier = Capture "identifier" UUID

type ErrorRoute = WithCookie :> WithReferer :> QueryParam "code" Int :> QueryParam "message" Text :> HtmlDoc
