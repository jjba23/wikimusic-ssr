{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.ErrorHtml where

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
import WikiMusic.SSR.View.HtmlUtil

errorPage' :: (MonadIO m) => Env -> ViewVars -> Text -> m Html
errorPage' env vv message' =
  simplePage env vv (SimplePageTitle $ (^. #titles % #songsPage) |##| (vv ^. #language)) $ section $ do
    h3 . text $ messageCauses
    H.pre ! class_ "font-size-small" $ text message
  where
    messageCauses :: Text
    messageCauses = T.intercalate " - " causeStrings
    message = decodeUtf8 . B16.decodeLenient . encodeUtf8 $ message'
    causeStrings = catMaybes [Just "Error", if T.isInfixOf "504" message then Just "Gateway Timeout" else Nothing]
