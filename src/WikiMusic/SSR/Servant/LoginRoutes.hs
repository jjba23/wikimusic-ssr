{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.Servant.LoginRoutes where

import Control.Monad.Error.Class
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text qualified as T
import Free.AlaCarte
import Optics
import Relude
import Servant
import Servant.Multipart
import Text.Blaze.Html as Html
import WikiMusic.Interaction.Model.User
import WikiMusic.Model.Auth
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Free.Backend
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

submitLoginRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
submitLoginRoute env multipartData = do
  maybeAuthToken <- liftIO $ exec @Backend (login env (LoginRequest {wikimusicEmail = email, wikimusicPassword = password}))
  case maybeAuthToken of
    Left e -> do
      _ <- liftIO $ BL.putStr . fromString . T.unpack $ e
      setCookieRoute (env ^. #cfg % #cookie) "/login" Map.empty
    Right authToken -> setCookieRoute (env ^. #cfg % #cookie) "/songs" (Map.fromList [(authCookieName, encodeToken authToken)])
  where
    email = T.unpack $ fromForm multipartData "" "email"
    password = T.unpack $ fromForm multipartData "" "password"

loginFormRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
loginFormRoute env cookie = liftIO $ exec @View (loginPage env vv)
  where
    vv = vvFromCookies cookie

doPasswordResetFormRoute :: (MonadIO m) => Env -> Maybe Text -> Maybe Text -> m Html
doPasswordResetFormRoute env cookie maybeToken = liftIO $ exec @View (doPasswordResetPage env vv maybeToken)
  where
    vv = vvFromCookies cookie

requestPasswordResetRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
requestPasswordResetRoute env cookie = liftIO $ exec @View (requestPasswordResetPage env vv)
  where
    vv = vvFromCookies cookie

doRequestPasswordResetRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
doRequestPasswordResetRoute env multipartData = do
  _ <-
    liftIO
      $ exec @Backend
        ( resetPassword
            env
            email
        )
  respondWithHttp
    httpFound
      { cause = Just "Requested password reset!",
        headers = [withLocation ("/login?email=" <> email)]
      }
  where
    email = fromForm multipartData "" "email"

doPasswordResetRoute :: (MonadIO m, MonadError ServerError m) => Env -> MultipartData tag -> m a
doPasswordResetRoute env multipartData = do
  _ <-
    liftIO
      $ exec @Backend
        ( resetPasswordDo
            env
            ( DoPasswordResetRequest
                { email = email,
                  token = token,
                  password = password,
                  passwordConfirm = passwordConfirm
                }
            )
        )
  respondWithHttp
    httpFound
      { cause = Just "Reset password!",
        headers = [withLocation ("/login?email=" <> email)]
      }
  where
    email = fromForm multipartData "" "email"
    password = fromForm multipartData "" "password"
    passwordConfirm = fromForm multipartData "" "passwordConfirm"
    token = fromForm multipartData "" "token"

inviteUsersRoute :: (MonadIO m) => Env -> Maybe Text -> m Html
inviteUsersRoute env cookie = liftIO $ exec @View (inviteUsersPage env vv)
  where
    vv = vvFromCookies cookie

inviteUsersFormRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> MultipartData tag -> m a
inviteUsersFormRoute env cookie multipartData = do
  _ <-
    liftIO
      $ exec @Backend
        ( userInvite
            env
            (vv ^. #authToken)
            InviteUsersRequest {..}
        )
  respondWithHttp
    httpFound
      { cause = Just "Requested password reset!",
        headers = [withLocation ("/login?email=" <> email)]
      }
  where
    vv = vvFromCookies cookie
    email = fromForm multipartData "" "email"
    displayName = fromForm multipartData "" "displayName"
    description = maybeFromForm multipartData "description"
    role = (read . T.unpack $ fromForm multipartData "" "role") :: UserRole
