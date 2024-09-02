module Main (main) where

import Relude
import WikiMusic.SSR.Boot qualified

main :: (MonadIO m) => m ()
main = liftIO WikiMusic.SSR.Boot.boot
