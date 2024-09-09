module Principium
  ( --
    module Relude,
    module Optics,
    module WikiMusic.Model.Other,
    module NeatInterpolation,
    BlazeHtml.Html,
    module WikiMusic.SSR.Language,
    module WikiMusic.SSR.Model.Api,
    module WikiMusic.SSR.Model.Env,
    module Free.AlaCarte,
    module WikiMusic.SSR.Model.Config,
    module Data.Time,
    module WikiMusic.SSR.View.Classes.Classes,
    UUID.UUID,
    --
    maybeDecodeUtf8,
    BL.ByteString,
    fromTextToAttributeValue,
    maybeDecodeBase16,
    uuidToText,
    intToText,
    unpackText,
    packText,
    filterText,
    MonadError,
    replaceText,
    mapElems,
    mapFromList,
    emptyMap,
    (Map.!?),
    mapFilter,
  )
where

--

--

import Control.Monad.Error.Class (MonadError)
import Data.ByteString.Base16.Lazy qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Free.AlaCarte
import NeatInterpolation hiding (text)
import Optics hiding (uncons)
import Relude hiding (ByteString)
import Text.Blaze.Html as BlazeHtml
import WikiMusic.Model.Other
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.Model.Config
import WikiMusic.SSR.Model.Env
import WikiMusic.SSR.View.Classes.Classes

--

maybeDecodeUtf8 = decodeUtf8'

fromTextToAttributeValue :: Text -> BlazeHtml.AttributeValue
fromTextToAttributeValue = fromString . T.unpack

maybeDecodeBase16 :: Text -> Either String BL.ByteString
maybeDecodeBase16 = B16.decode . fromString . T.unpack

uuidToText :: UUID.UUID -> Text
uuidToText = UUID.toText

intToText :: Int -> Text
intToText = T.pack . show

unpackText :: Text -> String
unpackText = T.unpack

packText :: String -> Text
packText = T.pack

filterText :: (Char -> Bool) -> Text -> Text
filterText = T.filter

replaceText :: Text -> Text -> Text -> Text
replaceText = T.replace

mapElems :: Map k a -> [a]
mapElems = Map.elems

mapFromList :: (Ord a) => [(a, b)] -> Map a b
mapFromList = Map.fromList

emptyMap :: Map k a
emptyMap = Map.empty

mapFilter :: (a -> Bool) -> Map k a -> Map k a
mapFilter = Map.filter
