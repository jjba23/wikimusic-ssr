{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Boot (boot) where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (ApacheLogger, withStdoutLogger)
import Principium
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import WikiMusic.SSR.Config
import WikiMusic.SSR.Servant.ApiSetup

boot :: (MonadIO m) => m ()
boot = liftIO $ withStdoutLogger $ \logger' ->
  ( do
      _ <- liftIO $ P.register P.ghcMetrics
      args <- liftIO getArgs
      maybeCfg <- readConfig (cfg args)
      liftIO $ either crashWithBadConfig (startWikiMusicSSR logger') maybeCfg
  )
  where
    crashWithBadConfig e = error ("Bad config could not be parsed! " <> show e)
    cfg args = case nonEmpty args of
      Just (x :| []) -> packText x
      _ -> "resources/config/run-local.toml"

startWikiMusicSSR :: (MonadIO m) => ApacheLogger -> AppConfig -> m ()
startWikiMusicSSR logger' cfg = do
  liftIO . putTextLn $ "Starting WikiMusic SSR..."
  app <- liftIO $ mkApp logger' cfg
  liftIO $ runSettings apiSettings app
  where
    apiSettings = setPort (cfg ^. #servant % #port) defaultSettings
