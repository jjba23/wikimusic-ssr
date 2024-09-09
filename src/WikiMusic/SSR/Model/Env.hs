{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Model.Env where

import Data.Text
import Data.Time
import Network.Wai.Logger (ApacheLogger)
import Optics
import Servant.Client
import WikiMusic.SSR.Model.Config

data Env = Env
  { logger :: ApacheLogger,
    cfg :: AppConfig,
    processStartedAt :: ZonedTime,
    reportedVersion :: Text,
    clientEnv :: ClientEnv
  }

makeFieldLabelsNoPrefix ''Env
