{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WikiMusic.SSR.Servant.Utilities where

import Data.ByteString.Base16.Lazy qualified as B16
import Data.Map qualified as Map
import Data.Text qualified as T
import NeatInterpolation
import Principium
import Servant
import Servant.Multipart
import Text.Blaze.Html.Renderer.Utf8
import WikiMusic.SSR.Backend.Rest ()
import WikiMusic.SSR.Free.View
import WikiMusic.SSR.View.Html ()

fromForm :: MultipartData tag -> Text -> Text -> Text
fromForm multipart fallback name =
  maybe fallback head
    . nonEmpty
    . map iValue
    . filter (\i -> iName i == name)
    $ inputs multipart

maybeFromForm :: MultipartData tag -> Text -> Maybe Text
maybeFromForm multipart name = case rawVal of
  (Just "") -> Nothing
  (Just x) -> Just x
  Nothing -> Nothing
  where
    rawVal =
      fmap head
        . nonEmpty
        . map iValue
        . filter (\i -> iName i == name)
        $ inputs multipart

setCookieRoute :: (MonadError ServerError m) => CookieConfig -> Text -> Map Text Text -> m a
setCookieRoute cookieConfig newLocation cookieMap =
  throwError
    $ ServerError
      { errHTTPCode = 302,
        errReasonPhrase = "Found",
        errBody = "",
        errHeaders =
          ("Location", encodeUtf8 newLocation) : cookieHeaders
      }
  where
    mkCookieHeaders (cookieName, cookieValue) =
      ( "Set-Cookie",
        fromString
          . T.unpack
          . mkCookieData cookieConfig
          $ [trimming|$cookieName=$cookieValue|]
      )
    cookieHeaders = map mkCookieHeaders (Map.assocs cookieMap)

mkCookieData :: CookieConfig -> Text -> Text
mkCookieData cookieConfig dyn =
  [trimming|
    $dyn; HttpOnly; $sameSite; Domain=$domain; Path=/; Max-Age=$maxAge $secureSuffix
  |]
  where
    maxAge = show $ cookieConfig ^. #maxAge
    domain = cookieConfig ^. #domain
    sameSite = cookieConfig ^. #sameSite
    secureSuffix = if cookieConfig ^. #secure then "; Secure" else ""

mkCookieMap :: Maybe Text -> Map Text Text
mkCookieMap cookie = do
  let diffCookies = maybe [] (T.splitOn "; ") cookie
      cookieParser [a, b] = Just (a, b)
      cookieParser _ = Nothing
      cookieMap = Map.fromList $ mapMaybe (cookieParser . T.splitOn "=") diffCookies
  cookieMap

decodeToken :: Text -> Text
decodeToken = decodeUtf8 . B16.decodeLenient . encodeUtf8

encodeToken :: Text -> Text
encodeToken = decodeUtf8 . B16.encode . encodeUtf8

vvFromCookies :: Maybe Text -> ViewVars
vvFromCookies cookie = ViewVars {..}
  where
    cookieMap = mkCookieMap cookie
    language = Language {value = fromMaybe "en" (cookieMap !? localeCookieName)}
    uiMode = UiMode {value = fromMaybe "light" (cookieMap !? uiModeCookieName)}
    authToken = AuthToken {value = decodeToken $ fromMaybe "" (cookieMap !? authCookieName)}
    songAsciiSize = SongAsciiSize {value = fromMaybe "medium" (cookieMap !? songAsciiSizeCookieName)}
    artistSorting =
      SortOrder
        { value = fromMaybe "created-at-desc" (cookieMap !? artistSortingCookieName)
        }
    songSorting =
      SortOrder
        { value = fromMaybe "created-at-desc" (cookieMap !? songSortingCookieName)
        }
    genreSorting =
      SortOrder
        { value = fromMaybe "created-at-desc" (cookieMap !? genreSortingCookieName)
        }
    palette = Palette {value = fromMaybe "mauve" (cookieMap !? paletteCookieName)}

errorRoute :: (MonadIO m, MonadError ServerError m) => Env -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> m Html
errorRoute env cookie _ maybeCode maybeMessage = do
  h <- liftIO $ exec @View (errorPage env vv maybeCode maybeMessage)
  throwError
    $ ServerError
      { errHTTPCode = fromMaybe 500 maybeCode,
        errReasonPhrase = "Error!",
        errBody = renderHtml h,
        errHeaders = []
      }
  where
    vv = vvFromCookies cookie

data ServerResponse = ServerResponse
  { code :: Int,
    cause :: Maybe Text,
    body :: Maybe Text,
    headers :: [(Text, Text)]
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''ServerResponse

serverResponse :: ServerResponse
serverResponse =
  ServerResponse
    { code = 200,
      cause = Just "OK",
      body = Nothing,
      headers = []
    }

withLocation :: (IsString a, IsString b) => Text -> (a, b)
withLocation location = ("Location", fromString . T.unpack $ location)

respondWithHttp :: (MonadIO m, MonadError ServerError m) => ServerResponse -> m a
respondWithHttp sr =
  throwError
    $ ServerError
      { errHTTPCode = sr ^. #code,
        errReasonPhrase = T.unpack $ fromMaybe "" $ sr ^. #cause,
        errBody = encodeUtf8 $ fromMaybe "" $ sr ^. #body,
        errHeaders =
          map
            ( bimap
                (fromString . unpackText)
                (fromString . unpackText)
            )
            (sr ^. #headers)
      }

httpFound :: ServerResponse
httpFound = serverResponse {code = 302}

callErrorPage :: (MonadIO m, MonadError ServerError m) => Text -> m a
callErrorPage e = respondWithHttp httpFound {cause = Just "Error occured!", headers = [withLocation newLocation]}
  where
    newLocation =
      "/error?code="
        <> ( T.pack
               . show
               $ 500
           )
        <> "&message="
        <> ( decodeUtf8
               . B16.encode
               . fromString
               . T.unpack
               $ e
           )

respondWithViewOrErr :: (MonadIO m, MonadError ServerError m) => Either Text t -> (t -> IO a) -> m a
respondWithViewOrErr x eff = case x of
  Left e -> callErrorPage e
  Right r -> liftIO $ eff r

respondWithViewOrErr' :: (MonadIO m, MonadError ServerError m) => Either Text (Maybe t) -> (t -> IO a) -> m a
respondWithViewOrErr' x eff = case x of
  Left e -> callErrorPage e
  Right r -> case r of
    Nothing -> callErrorPage "Error!"
    Just item -> liftIO $ eff item
