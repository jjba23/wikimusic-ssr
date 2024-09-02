{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.Model.Config
  ( AppConfig (..),
    ServantConfig (..),
    CorsConfig (..),
    CookieConfig (..),
    WebFrontendConfig (..),
    appConfigCodec,
  )
where

import Optics
import Relude
import Toml

data ServantConfig = ServantConfig
  { port :: Int,
    host :: Text
  }
  deriving (Generic, Eq, Show)

servantConfigCodec :: TomlCodec ServantConfig
servantConfigCodec =
  ServantConfig
    <$> Toml.int "port"
    .= (^. #port)
    <*> Toml.text "host"
    .= (^. #host)

data CorsConfig = CorsConfig
  { origins :: [Text],
    methods :: [Text],
    requestHeaders :: [Text]
  }
  deriving (Generic, Eq, Show)

corsConfigCodec :: TomlCodec CorsConfig
corsConfigCodec =
  CorsConfig
    <$> Toml.arrayOf Toml._Text "origins"
    .= (^. #origins)
    <*> Toml.arrayOf Toml._Text "methods"
    .= (^. #methods)
    <*> Toml.arrayOf Toml._Text "request-headers"
    .= (^. #requestHeaders)

data CookieConfig = CookieConfig
  { maxAge :: Int,
    path :: Text,
    domain :: Text,
    secure :: Bool,
    sameSite :: Text
  }
  deriving (Generic, Eq, Show)

cookieConfigCodec :: TomlCodec CookieConfig
cookieConfigCodec =
  CookieConfig
    <$> Toml.int "max-age"
    .= (^. #maxAge)
    <*> Toml.text "path"
    .= (^. #path)
    <*> Toml.text "domain"
    .= (^. #domain)
    <*> Toml.bool "secure"
    .= (^. #secure)
    <*> Toml.text "same-site"
    .= (^. #sameSite)

newtype DevConfig = DevConfig
  { reportedVersion :: Text
  }
  deriving (Generic, Eq, Show)

devCodec :: TomlCodec DevConfig
devCodec = DevConfig <$> Toml.text "reported-version" .= (^. #reportedVersion)

newtype WebFrontendConfig = WebFrontendConfig
  { baseUrl :: Text
  }
  deriving (Generic, Eq, Show)

webFrontendConfigCodec :: TomlCodec WebFrontendConfig
webFrontendConfigCodec =
  WebFrontendConfig
    <$> Toml.text "base-url"
    .= (^. #baseUrl)

data ApiConfig = ApiConfig
  { host :: Text,
    port :: Int,
    protocol :: Text
  }
  deriving (Generic, Eq, Show)

apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec =
  ApiConfig
    <$> Toml.text "host"
    .= (^. #host)
    <*> Toml.int "port"
    .= (^. #port)
    <*> Toml.text "protocol"
    .= (^. #protocol)

data AppConfig = AppConfig
  { servant :: ServantConfig,
    cors :: CorsConfig,
    cookie :: CookieConfig,
    webFrontend :: WebFrontendConfig,
    dev :: DevConfig,
    api :: ApiConfig
  }
  deriving (Generic, Eq, Show)

appConfigCodec :: TomlCodec AppConfig
appConfigCodec =
  AppConfig
    <$> Toml.table servantConfigCodec "servant"
    .= (^. #servant)
    <*> Toml.table corsConfigCodec "cors"
    .= (^. #cors)
    <*> Toml.table cookieConfigCodec "cookie"
    .= (^. #cookie)
    <*> Toml.table webFrontendConfigCodec "web-frontend"
    .= (^. #webFrontend)
    <*> Toml.table devCodec "dev"
    .= (^. #dev)
    <*> Toml.table apiConfigCodec "api"
    .= (^. #api)

makeFieldLabelsNoPrefix ''AppConfig
makeFieldLabelsNoPrefix ''ServantConfig
makeFieldLabelsNoPrefix ''CorsConfig
makeFieldLabelsNoPrefix ''CookieConfig
makeFieldLabelsNoPrefix ''WebFrontendConfig
makeFieldLabelsNoPrefix ''DevConfig
makeFieldLabelsNoPrefix ''ApiConfig
