{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- use dark: in tailwind to use system dark / light mode, or use vv to read from cookie
simpleEntityCard vv path entity = article
  ! css'
    [ "bg-slate-100",
      "rounded-2xl",
      "flex",
      "flex-wrap",
      "gap-4",
      "flex-row",
      "md:flex-col",
      "max-w-56",
      "border",
      if vv ^. #uiMode % #value == "dark" then "dark:bg-black/70" else "bg-white/80",
      "border-" <> vv ^. #palette % #value <> "-300"
    ]
  $ do
    maybeImg
    H.div ! css' ["px-4", "py-4", "flex", "flex-col", "gap-4", "align-center"] $ do
      a
        ! href (mkIdentifierHref path (entity ^. #identifier))
        $ (h3 ! css' ["text-xl", "font-bold", "break-words", "text-center", if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"])
        . text
        $ entity
        ^. #displayName
      detailList $ do
        detailListEntry vv ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount entity)
        detailListEntry vv ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount entity)
        detailListEntry vv ((^. #more % #views) |##| (vv ^. #language)) (text . packText . show $ entity ^. #viewCount)
  where
    artworks = map (\x -> x ^. #artwork) (mapElems $ entity ^. #artworks) :: [Artwork]
    sortedArts = sortBy (\x y -> compare (x ^. #orderValue) (y ^. #orderValue)) artworks
    maybeImg = maybe (H.span "") (toImg . head) (nonEmpty sortedArts)
    toImg x =
      a
        ! href (mkIdentifierHref path (entity ^. #identifier))
        $ img
        ! css' ["object-cover", "w-60", "h-60", "rounded-2xl"]
        ! customAttribute "loading" "lazy"
        ! src (fromString . unpackText $ x ^. #contentUrl)

imageCarousel :: [Artwork] -> Html
imageCarousel artworks =
  section ! class_ "flex flex-wrap flex-col gap-6" $ do
    mapM_
      ( \x ->
          H.div $ do
            img
              ! css' ["object-cover", "w-72", "h-72", "rounded-2xl"]
              ! customAttribute "loading" "lazy"
              ! src (fromString . unpackText $ x ^. #contentUrl)
            mapM_ (H.span . text) (x ^. #contentCaption)
      )
      artworks

entityDetailsSkeleton :: Html -> Html -> Html
entityDetailsSkeleton slot0 slot1 =
  H.div ! css' ["flex", "flex-row", "flex-wrap", "gap-lg"] $ do
    H.div ! css' ["flex", "flex-col", "flex-wrap", "gap-lg", "w-full", "md:w-1/2"] $ do
      slot0
    H.div ! css' ["flex", "flex-col", "flex-wrap", "gap-lg", "w-full", "md:w-1/2"] $ do
      slot1

verboseLink' uri = a ! href (fromString . unpackText $ uri) $ text uri

entityLinks vv x = do
  mapM_
    (detailListEntry vv "Spotify" . verboseLink')
    (x ^. #spotifyUrl)
  mapM_
    (detailListEntry vv "Wikipedia" . verboseLink')
    (x ^. #wikipediaUrl)
  mapM_
    (detailListEntry vv "YouTube" . verboseLink')
    (x ^. #youtubeUrl)
  mapM_
    (detailListEntry vv "SoundCloud" . verboseLink')
    (x ^. #soundcloudUrl)

slot0 x = do
  imageCarousel (map (^. #artwork) (mapElems $ x ^. #artworks))
  mapM_ ((p ! A.class_ "white-space-break-spaces") . text) (x ^. #description)

slot1 vv path x = section
  ! css'
    [ "flex",
      "flex-col",
      "flex-wrap",
      "items-center",
      "gap-8"
    ]
  $ do
    ( h3
        ! css'
          [ "text-3xl",
            "text-black",
            "font-bold",
            if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"
          ]
      )
      . fromString
      . unpackText
      $ (x ^. #displayName)
    H.div ! class_ "flex flex-row flex-wrap justify-center gap-md" $ do
      likesDislikes vv path' x
      entityButtons vv path' x

      section
        ! class_ "flex flex-row flex-wrap justify-center gap-md"
        $ detailList
        $ do
          entityBaseDetails vv x
      hr
      entityLinks vv x
  where
    path' = unpackText path

entityDetails vv path x = do
  entityDetailsSkeleton (slot0 x) (slot1 vv path x)

likesDislikes vv path' x = do
  postForm (fromString ("/" <> path' <> "/like/" <> show (x ^. #identifier))) $ do
    button ! css (cssButton vv) ! type_ "submit" $ do
      H.span "+"
      text ((^. #buttons % #like) |##| (vv ^. #language))
  postForm (fromString ("/" <> path' <> "/dislike/" <> show (x ^. #identifier))) $ do
    button ! css (cssButton vv) ! type_ "submit" $ do
      H.span "-"
      text ((^. #buttons % #dislike) |##| (vv ^. #language))

entityBaseDetails vv x = do
  detailListEntry vv ((^. #more % #likes) |##| (vv ^. #language)) (text $ likeCount x)
  detailListEntry vv ((^. #more % #dislikes) |##| (vv ^. #language)) (text $ dislikeCount x)
  detailListEntry vv ((^. #more % #views) |##| (vv ^. #language)) (show $ x ^. #viewCount)
  detailListEntry vv ((^. #more % #createdAt) |##| (vv ^. #language)) (show $ x ^. #createdAt)
  mapM_
    (detailListEntry vv ((^. #more % #lastEditedAt) |##| (vv ^. #language)))
    (show <$> x ^. #lastEditedAt)
  detailListEntry vv ((^. #more % #createdBy) |##| (vv ^. #language)) (show $ x ^. #createdBy)

entityButtons vv path' x = do
  a
    ! href (fromString ("/" <> path' <> "/edit/" <> show (x ^. #identifier)))
    $ button
    ! css (cssButton vv)
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
