{-# LANGUAGE OverloadedLabels #-}

module WikiMusic.SSR.Servant.ApiSetup (mkApp) where

import Data.Text qualified as T
import Data.Time
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Logger (ApacheLogger)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Prometheus qualified as P
import Network.Wai.Middleware.RequestLogger
import Optics
import Prometheus qualified as P
import Prometheus.Metric.GHC qualified as P
import Relude
import Servant
import Servant.Client
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Model.Config
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.Servant.ApiSpec
import WikiMusic.SSR.Servant.ArtistRoutes
import WikiMusic.SSR.Servant.GenreRoutes
import WikiMusic.SSR.Servant.LoginRoutes
import WikiMusic.SSR.Servant.PreferenceRoutes
import WikiMusic.SSR.Servant.SongRoutes
import WikiMusic.SSR.Servant.Utilities
import WikiMusic.SSR.View.Html ()

newClientEnv :: (MonadIO m) => AppConfig -> m ClientEnv
newClientEnv cfg = do
  manager <- liftIO $ newManager tlsManagerSettings
  pure $ clientEnv manager Nothing
  where
    baseUrl' =
      BaseUrl
        { baseUrlScheme = if (cfg ^. #api % #protocol) == "https" then Https else Http,
          baseUrlHost = T.unpack $ cfg ^. #api % #host,
          baseUrlPort = cfg ^. #api % #port,
          baseUrlPath = ""
        }
    clientEnv manager cookieJar =
      ClientEnv
        { manager = manager,
          baseUrl = baseUrl',
          cookieJar = cookieJar,
          makeClientRequest = defaultMakeClientRequest,
          middleware = id
        }

mkApp :: ApacheLogger -> AppConfig -> IO Application
mkApp logger' cfg = do
  let apiCfg = EmptyContext
  now <- getZonedTime
  mainCss <- liftIO (readFileBS "resources/css/main.css")
  lightCss <- liftIO (readFileBS "resources/css/light.css")
  darkCss <- liftIO (readFileBS "resources/css/dark.css")
  greenPaletteCss <- liftIO (readFileBS "resources/css/palettes/green.css")
  mauvePaletteCss <- liftIO (readFileBS "resources/css/palettes/mauve.css")
  clientEnv <- newClientEnv cfg
  let env =
        Env
          { logger = logger',
            cfg = cfg,
            processStartedAt = now,
            reportedVersion = cfg ^. #dev % #reportedVersion,
            mainCss = prepareCSS mainCss,
            darkCss = prepareCSS darkCss,
            lightCss = prepareCSS lightCss,
            clientEnv = clientEnv,
            palettes =
              PalettesCss
                { green = prepareCSS greenPaletteCss,
                  mauve = prepareCSS mauvePaletteCss
                }
          }
  pure
    . (if (cfg ^. #dev % #reportedVersion) == "dev" then logStdoutDev else logStdout)
    . myCors (cfg ^. #cors)
    . P.prometheus P.def
    $ serveWithContext wikimusicSSRServant apiCfg (server env)
  where
    prepareCSS = T.filter (\x -> x /= '\n' && x /= '\t') . decodeUtf8

artistBaseEntityRoutes :: Env -> Server BaseEntityRoutes
artistBaseEntityRoutes env =
  artistsRoute env
    :<|> artistRoute env
    :<|> artistCreateRoute env
    :<|> artistCreateFormRoute env
    :<|> artistLikeRoute env
    :<|> artistDislikeRoute env
    :<|> artistEditRoute env
    :<|> artistEditFormRoute env
    :<|> searchArtistRoute env
    :<|> (createArtistArtworkRoute env :<|> artistArtworkDeleteRoute env :<|> updateArtistArtworkOrderRoute env)
    :<|> artistDeleteRoute env

genreBaseEntityRoutes :: Env -> Server BaseEntityRoutes
genreBaseEntityRoutes env =
  genresRoute env
    :<|> genreRoute env
    :<|> genreCreateRoute env
    :<|> genreCreateFormRoute env
    :<|> genreLikeRoute env
    :<|> genreDislikeRoute env
    :<|> genreEditRoute env
    :<|> genreEditFormRoute env
    :<|> searchGenreRoute env
    :<|> (createGenreArtworkRoute env :<|> genreArtworkDeleteRoute env :<|> updateGenreArtworkOrderRoute env)
    :<|> genreDeleteRoute env

songBaseEntityRoutes :: Env -> Server BaseEntityRoutes
songBaseEntityRoutes env =
  songsRoute env
    :<|> songRoute env
    :<|> songCreateRoute env
    :<|> songCreateFormRoute env
    :<|> songLikeRoute env
    :<|> songDislikeRoute env
    :<|> songEditRoute env
    :<|> songEditFormRoute env
    :<|> searchSongRoute env
    :<|> (createSongArtworkRoute env :<|> songArtworkDeleteRoute env :<|> updateSongArtworkOrderRoute env)
    :<|> songDeleteRoute env

preferenceRoutes :: Env -> Server PreferenceRoutes
preferenceRoutes env =
  setLanguageRoute env
    :<|> setArtistSortingRoute env
    :<|> setGenreSortingRoute env
    :<|> setSongSortingRoute env
    :<|> setDarkModeRoute env
    :<|> setSongAsciiSizeRoute env
    :<|> setPaletteRoute env

loginRoutes :: Env -> Server LoginRoutes
loginRoutes env =
  loginFormRoute env
    :<|> submitLoginRoute env

songContentsRoutes :: Env -> Server SongContentsRoutes
songContentsRoutes env =
  songContentCreateFormRoute env
    :<|> songContentEditFormRoute env
    :<|> songContentDeleteRoute env

passwordRoutes :: Env -> Server PasswordRoutes
passwordRoutes env =
  requestPasswordResetRoute env
    :<|> doRequestPasswordResetRoute env
    :<|> doPasswordResetFormRoute env
    :<|> doPasswordResetRoute env

userRoutes :: Env -> Server UserRoutes
userRoutes env =
  inviteUsersRoute env
    :<|> inviteUsersFormRoute env

server :: Env -> Server WikiMusicSSRServant
server env =
  fallbackRoute
    :<|> artistBaseEntityRoutes env
    :<|> genreBaseEntityRoutes env
    :<|> ( songBaseEntityRoutes env
             :<|> songContentsRoutes env
         )
    :<|> preferenceRoutes env
    :<|> loginRoutes env
    :<|> errorRoute env
    :<|> passwordRoutes env
    :<|> userRoutes env

fallbackRoute :: Handler a
fallbackRoute =
  respondWithHttp
    httpFound
      { cause = Just "Fallback page!",
        headers =
          [ withLocation "/songs"
          ]
      }

wikimusicSSRServant :: Proxy WikiMusicSSRServant
wikimusicSSRServant = Proxy

myCors :: CorsConfig -> Middleware
myCors cfg = cors (const $ Just policy)
  where
    policy =
      CorsResourcePolicy
        { corsOrigins = Just (map (fromString . T.unpack) (cfg ^. #origins), True),
          corsMethods = map (fromString . T.unpack) (cfg ^. #methods),
          corsRequestHeaders = map (fromString . T.unpack) (cfg ^. #requestHeaders),
          corsExposedHeaders =
            Just
              [ "content-type",
                "date",
                "content-length",
                "access-control-allow-origin",
                "access-control-allow-methods",
                "access-control-allow-headers",
                "access-control-request-method",
                "access-control-request-headers"
              ],
          corsMaxAge = Nothing,
          corsVaryOrigin = False,
          corsRequireOrigin = False,
          corsIgnoreFailures = False
        }
