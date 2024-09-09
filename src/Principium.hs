{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Principium
  ( --
    module Relude,
    module Optics,
    module Text.Blaze.Html,
    module WikiMusic.SSR.Language,
    module WikiMusic.SSR.Model.Api,
    module WikiMusic.SSR.Model.Env,
    --
    maybeDecodeBase16,
    maybeDecodeUtf8,
    BS.ByteString,
  )
where

--

--

import Data.ByteString qualified as BS
import Data.ByteString.Base16.Lazy qualified as B16
import Optics hiding (uncons)
import Relude hiding (ByteString)
import Text.Blaze.Html
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Env

maybeDecodeBase16 = B16.decode

maybeDecodeUtf8 :: BS.ByteString -> Either UnicodeException Text
maybeDecodeUtf8 = decodeUtf8'
