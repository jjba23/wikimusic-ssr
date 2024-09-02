{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Config (readConfig) where

import Data.Text (unpack)
import Relude
import Toml
import WikiMusic.SSR.Model.Config

readConfig :: (MonadIO m) => Text -> m (Either Text AppConfig)
readConfig filePath = do
  parseResult <- liftIO $ decodeFileEither appConfigCodec (unpack filePath)
  case parseResult of
    Left e -> pure . Left $ prettyTomlDecodeErrors e
    Right r -> pure . Right $ r
