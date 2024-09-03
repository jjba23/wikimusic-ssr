{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.OtherHtml where

import Data.ByteString.Base16.Lazy qualified as B16
import Data.Text qualified as T
import Optics
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.View.Components.Forms
import WikiMusic.SSR.View.HtmlUtil

errorPage' :: (MonadIO m) => Env -> ViewVars -> Maybe Int -> Maybe Text -> m Html
errorPage' env vv _ maybeMessage = do
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ section $ do
    h3 . text $ messageCauses
    H.pre
      ! class_ "font-size-small"
      $ text (maybe "Error ocurred!" (decodeUtf8 . B16.decodeLenient . fromString . T.unpack) maybeMessage)
  where
    messageCauses = T.intercalate " - " causeStrings
    causeStrings = catMaybes [Just "Error", if T.isInfixOf "504" (fromMaybe "Error ocurred!" maybeMessage) then Just "Gateway Timeout" else Nothing]

loginPage' :: (MonadIO m) => Env -> ViewVars -> m Html
loginPage' env vv = do
  simplePage env vv (SimplePageTitle $ (^. #more % #loginNav) |##| (vv ^. #language)) $ do
    section $ postForm "/login" $ do
      requiredEmailInput "email" ((^. #forms % #email) |##| (vv ^. #language))
      requiredPasswordInput "password" ((^. #forms % #password) |##| (vv ^. #language))
      submitButton vv
    a ! class_ "accent-color" ! href "/passwords/request-reset" $ "forgot password ?"

doPasswordResetPage' :: (MonadIO m) => Env -> ViewVars -> Maybe Text -> m Html
doPasswordResetPage' env vv t = do
  simplePage env vv (SimplePageTitle "Reset password") $ do
    section $ postForm "/passwords/do-reset" $ do
      requiredEmailInput "email" "email"
      requiredPasswordInput "password" "password"
      requiredPasswordInput "passwordConfirm" "password again"
      requiredTextInput' "token" "token" t
      submitButton vv

requestPasswordResetPage' :: (MonadIO m) => Env -> ViewVars -> m Html
requestPasswordResetPage' env vv = do
  simplePage env vv (SimplePageTitle "Request password reset ?") $ do
    section $ postForm "/passwords/request-reset" $ do
      requiredEmailInput "email" "email"
      submitButton vv

inviteUserPage' :: (MonadIO m) => Env -> ViewVars -> m Html
inviteUserPage' env vv = do
  simplePage env vv (SimplePageTitle "Invite user") $ do
    section $ postForm "/users/invite" $ do
      requiredEmailInput "email" "email"
      requiredTextInput "displayName" "name"
      H.label ! for "role" $ "role"
      select ! required "" ! name "role" ! A.id "role" $ do
        option ! value "wm::demo" $ "demo user"
        option ! value "wm::lowrank" $ "average user"
        option ! value "wm::maintainer" $ "wiki maintainer"
        option ! value "wm::superuser" $ "super user"
      optionalTextArea "description" "description"
      submitButton vv


