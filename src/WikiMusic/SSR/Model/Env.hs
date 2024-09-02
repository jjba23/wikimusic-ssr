{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Model.Env
  ( Env (..),
    PalettesCss (..),
  )
where

import Data.Text
import Data.Time
import Network.Wai.Logger (ApacheLogger)
import Optics
import Servant.Client
import WikiMusic.SSR.Model.Config

data PalettesCss = PalettesCss
  { green :: Text,
    mauve :: Text
  }

makeFieldLabelsNoPrefix ''PalettesCss

data Env = Env
  { logger :: ApacheLogger,
    cfg :: AppConfig,
    processStartedAt :: ZonedTime,
    reportedVersion :: Text,
    mainCss :: Text,
    darkCss :: Text,
    lightCss :: Text,
    clientEnv :: ClientEnv,
    palettes :: PalettesCss
  }

makeFieldLabelsNoPrefix ''Env
