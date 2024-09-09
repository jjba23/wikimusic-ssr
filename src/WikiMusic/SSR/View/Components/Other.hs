{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module WikiMusic.SSR.View.Components.Other
  ( likeCount,
    dislikeCount,
    simpleEntityCard,
    imageCarousel,
    warningBanner,
    entityDetails,
  )
where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UUID (UUID)
import Optics
import Relude
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Model.Artwork
import WikiMusic.SSR.Language
import WikiMusic.SSR.Model.Api
import WikiMusic.SSR.View.Components.DetailList
import WikiMusic.SSR.View.Components.Forms

likeCount entity =
  T.pack
    . show
    . length
    $ Map.elems
    $ Map.filter (^. #opinion % #isLike) (entity ^. #opinions)

dislikeCount entity =
  T.pack
    . show
    . length
    $ Map.elems
    $ Map.filter (^. #opinion % #isDislike) (entity ^. #opinions)

mkIdentifierHref :: Text -> UUID -> AttributeValue
mkIdentifierHref path identifier = fromString ("/" <> T.unpack path <> "/" <> show identifier)

simpleEntityCard vv path entity = article $ do
  maybeImg
  a
    ! href (mkIdentifierHref path (entity ^. #identifier))
    ! class_ "margin-top-large"
    $ (h3 ! class_ "highlight-header text-align-center")
    . text
    $ entity
    ^. #displayName
  detailList $ do
    detailListEntry ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount entity)
    detailListEntry ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount entity)
    detailListEntry ((^. #more % #views) |##| (vv ^. #language)) (text $ T.pack . show $ entity ^. #viewCount)
  where
    artworks = Relude.map (\x -> x ^. #artwork) (Map.elems $ entity ^. #artworks) :: [Artwork]
    sortedArts = sortBy (\x y -> compare (x ^. #orderValue) (y ^. #orderValue)) artworks
    maybeImg = maybe (H.span "") (toImg . Relude.head) (nonEmpty sortedArts)
    toImg x =
      a
        ! href (mkIdentifierHref path (entity ^. #identifier))
        $ img
        ! class_ "object-cover w-60 h-60 rounded-2xl"
        ! customAttribute "loading" "lazy"
        ! src (fromString . T.unpack $ x ^. #contentUrl)

imageCarousel :: [Artwork] -> Html
imageCarousel artworks =
  section ! class_ "flex flex-wrap flex-col gap-6" $ do
    mapM_
      ( \x ->
          H.div $ do
            img
              ! class_ "object-cover w-80 h-80 rounded-2xl"
              ! customAttribute "loading" "lazy"
              ! src (fromString . T.unpack $ x ^. #contentUrl)
            mapM_ (H.span . text) (x ^. #contentCaption)
      )
      artworks

entityDetailsSkeleton :: Html -> Html -> Html
entityDetailsSkeleton slot0 slot1 =
  H.div $ do
    H.div $ do
      slot0
    H.div $ do
      slot1

entityDetails language path x =
  entityDetailsSkeleton slot0 slot1
  where
    verboseLink' uri = a ! href (fromString . T.unpack $ uri) $ text uri
    path' = T.unpack path
    entityLinks = do
      mapM_
        (detailListEntry "Spotify" . verboseLink')
        (x ^. #spotifyUrl)
      mapM_
        (detailListEntry "Wikipedia" . verboseLink')
        (x ^. #wikipediaUrl)
      mapM_
        (detailListEntry "YouTube" . verboseLink')
        (x ^. #youtubeUrl)
      mapM_
        (detailListEntry "SoundCloud" . verboseLink')
        (x ^. #soundcloudUrl)
    slot0 = do
      imageCarousel (Relude.map (^. #artwork) (Map.elems $ x ^. #artworks))
      mapM_ ((p ! A.class_ "white-space-break-spaces") . text) (x ^. #description)

    slot1 = do
      (h3 ! class_ "text-align-center font-size-xxx-large font-weight-500") . fromString . T.unpack $ (x ^. #displayName)
      H.div $ do
        likesDislikes path' language x
        entityButtons path' language x

        section
          ! class_ "flex direction-row justify-content-center gap-small align-items-baseline"
          $ detailList
          $ do
            entityBaseDetails language x
        hr
        entityLinks

likesDislikes path' vv x = do
  section $ do
    postForm' (fromString ("/" <> path' <> "/like/" <> show (x ^. #identifier))) "" $ do
      button ! type_ "submit" $ do
        H.span "+"
        text ((^. #buttons % #like) |##| (vv ^. #language))
    postForm' (fromString ("/" <> path' <> "/dislike/" <> show (x ^. #identifier))) "" $ do
      button ! type_ "submit" $ do
        H.span "-"
        text ((^. #buttons % #dislike) |##| (vv ^. #language))

entityBaseDetails vv x = do
  detailListEntry ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount x)
  detailListEntry ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount x)
  detailListEntry ((^. #more % #views) |##| (vv ^. #language)) (Relude.show $ x ^. #viewCount)
  detailListEntry ((^. #more % #createdAt) |##| (vv ^. #language)) (Relude.show $ x ^. #createdAt)
  mapM_
    (detailListEntry ((^. #more % #lastEditedAt) |##| (vv ^. #language)))
    (Relude.show <$> x ^. #lastEditedAt)
  detailListEntry ((^. #more % #createdBy) |##| (vv ^. #language)) (Relude.show $ x ^. #createdBy)

entityButtons path' vv x = do
  H.div $ do
    a
      ! href (fromString ("/" <> path' <> "/edit/" <> show (x ^. #identifier)))
      $ button
      $ text ((^. #buttons % #edit) |##| (vv ^. #language))
    dangerPostForm vv (fromString ("/" <> path' <> "/delete/" <> show (x ^. #identifier))) $ do
      deleteButton vv

warningBanner :: ViewVars -> Html
warningBanner vv =
  section $ do
    small
      ! class_ "warning-text"
      $ text
        ((^. #more % #warningHeavyDevelopment) |##| (vv ^. #language))
