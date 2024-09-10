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

import Principium
import Text.Blaze.Html5 as H hiding (head, map)
import Text.Blaze.Html5.Attributes as A
import WikiMusic.Model.Artwork
import WikiMusic.SSR.View.Components.DetailList
import WikiMusic.SSR.View.Components.Forms

likeCount entity =
  packText
    . show
    . length
    $ mapElems
    $ mapFilter (^. #opinion % #isLike) (entity ^. #opinions)

dislikeCount entity =
  packText
    . show
    . length
    $ mapElems
    $ mapFilter (^. #opinion % #isDislike) (entity ^. #opinions)

mkIdentifierHref :: Text -> UUID -> AttributeValue
mkIdentifierHref path identifier = fromString ("/" <> unpackText path <> "/" <> show identifier)

simpleEntityCard vv path entity = article ! class_ (["bg-slate-100", "rounded-2xl", "flex", "flex-wrap", "gap-4", "flex-row", "md:flex-col", "max-w-60", "border", "border-gray-300"] @@) $ do
  maybeImg
  H.div ! class_ "px-6 py-6" $ do
    a
      ! href (mkIdentifierHref path (entity ^. #identifier))
      $ (h3 ! class_ (["text-2xl", "font-bold", "break-words"] @@))
      . text
      $ entity
      ^. #displayName
    detailList (Just . Class $ "gap-4") $ do
      detailListEntry ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount entity)
      detailListEntry ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount entity)
      detailListEntry ((^. #more % #views) |##| (vv ^. #language)) (text . packText . show $ entity ^. #viewCount)
  where
    artworks = map (\x -> x ^. #artwork) (mapElems $ entity ^. #artworks) :: [Artwork]
    sortedArts = sortBy (\x y -> compare (x ^. #orderValue) (y ^. #orderValue)) artworks
    maybeImg = maybe (H.span "") (toImg . head) (nonEmpty sortedArts)
    toImg x =
      a
        ! href (mkIdentifierHref path (entity ^. #identifier))
        $ img
        ! class_ (["object-cover", "w-60", "h-60", "rounded-2xl"] @@)
        ! customAttribute "loading" "lazy"
        ! src (fromString . unpackText $ x ^. #contentUrl)

imageCarousel :: [Artwork] -> Html
imageCarousel artworks =
  section ! class_ "flex flex-wrap flex-col gap-6" $ do
    mapM_
      ( \x ->
          H.div $ do
            img
              ! class_ (["object-cover", "w-72", "h-72", "rounded-2xl"] @@)
              ! customAttribute "loading" "lazy"
              ! src (fromString . unpackText $ x ^. #contentUrl)
            mapM_ (H.span . text) (x ^. #contentCaption)
      )
      artworks

entityDetailsSkeleton :: Html -> Html -> Html
entityDetailsSkeleton slot0 slot1 =
  H.div ! class_ (["flex", "flex-row", "flex-wrap", "gap-lg"] @@) $ do
    H.div ! class_ (["flex", "flex-col", "flex-wrap", "gap-lg", "w-full", "md:w-1/2"] @@) $ do
      slot0
    H.div ! class_ (["flex", "flex-col", "flex-wrap", "gap-lg", "w-full", "md:w-1/2"] @@) $ do
      slot1

entityDetails language path x =
  entityDetailsSkeleton slot0 slot1
  where
    verboseLink' uri = a ! href (fromString . unpackText $ uri) $ text uri
    path' = unpackText path
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
      imageCarousel (map (^. #artwork) (mapElems $ x ^. #artworks))
      mapM_ ((p ! A.class_ "white-space-break-spaces") . text) (x ^. #description)

    slot1 = section ! class_ (["flex", "flex-col", "flex-wrap", "items-center", "gap-8"] @@) $ do
      (h3 ! class_ (["text-3xl", "text-black", "font-bold"] @@)) . fromString . unpackText $ (x ^. #displayName)
      H.div ! class_ "flex flex-row flex-wrap justify-center gap-md" $ do
        likesDislikes path' language x
        entityButtons path' language x

        section
          ! class_ "flex flex-row flex-wrap justify-center gap-md"
          $ detailList Nothing
          $ do
            entityBaseDetails language x
        hr
        entityLinks

likesDislikes path' vv x = do
  section $ do
    postForm' (fromString ("/" <> path' <> "/like/" <> show (x ^. #identifier))) "" $ do
      button ! class_ (fromTextToAttributeValue someButtonClass) ! type_ "submit" $ do
        H.span "+"
        text ((^. #buttons % #like) |##| (vv ^. #language))
    postForm' (fromString ("/" <> path' <> "/dislike/" <> show (x ^. #identifier))) "" $ do
      button ! class_ (fromTextToAttributeValue someButtonClass) ! type_ "submit" $ do
        H.span "-"
        text ((^. #buttons % #dislike) |##| (vv ^. #language))

entityBaseDetails vv x = do
  detailListEntry ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount x)
  detailListEntry ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount x)
  detailListEntry ((^. #more % #views) |##| (vv ^. #language)) (show $ x ^. #viewCount)
  detailListEntry ((^. #more % #createdAt) |##| (vv ^. #language)) (show $ x ^. #createdAt)
  mapM_
    (detailListEntry ((^. #more % #lastEditedAt) |##| (vv ^. #language)))
    (show <$> x ^. #lastEditedAt)
  detailListEntry ((^. #more % #createdBy) |##| (vv ^. #language)) (show $ x ^. #createdBy)

entityButtons path' vv x = do
  H.div $ do
    a
      ! href (fromString ("/" <> path' <> "/edit/" <> show (x ^. #identifier)))
      $ button
      ! class_ (fromTextToAttributeValue someButtonClass)
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
