{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.View.Css where

import Data.Text qualified as T
import Optics
import Relude
import Text.Blaze.Html qualified as BlazeHtml
import Text.Blaze.Html5.Attributes qualified as A

newtype Css = Css {className :: Text} deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''Css

cssToAttrValue :: [Text] -> BlazeHtml.AttributeValue
cssToAttrValue = fromString . T.unpack . joinClasses . fromList
  where
    joinClasses :: Set Text -> Text
    joinClasses = T.intercalate " " . toList

css' :: [Text] -> BlazeHtml.Attribute
css' xs = css (fromList xs :: Set Text)

css :: Set Text -> BlazeHtml.Attribute
css xs = A.class_ (cssToAttrValue $ toList xs)

cssSubmitButton :: Set Text
cssSubmitButton =
  fromList
    [ "text-white",
      "bg-green-700/75",
      "hover:bg-green-800",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-green-300",
      "font-medium",
      "rounded-2xl",
      "text-sm",
      "px-5",
      "py-2.5",
      "text-center",
      "me-2",
      "mb-2",
      "font-sans",
      "w-fit"
    ]

cssSelect :: Set Text
cssSelect =
  fromList
    [ "text-black",
      "bg-gray-100/75",
      "hover:bg-accent-500",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-gray-300",
      "font-medium",
      "rounded-2xl",
      "text-lg",
      "px-8",
      "py-2",
      "me-2",
      "mb-2",
      "font-sans",
      "w-fit",
      "h-fit",
      "border"
    ]

cssButton :: Set Text
cssButton =
  fromList
    [ "text-black",
      "bg-gray-100/75",
      "hover:bg-accent-500",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-gray-300",
      "font-medium",
      "rounded-2xl",
      "text-lg",
      "px-5",
      "py-2.5",
      "me-2",
      "mb-2",
      "font-sans",
      "w-fit",
      "border"
    ]

cssCenteredCardGrid :: Set Text
cssCenteredCardGrid = fromList ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center"]

cssInput :: Set Text
cssInput = fromList ["rounded-2xl", "px-5", "py-2.5", "font-sans"]

cssTextarea :: Set Text
cssTextarea = fromList ["rounded-2xl", "px-5", "py-2.5"]

cssDetails :: Set Text
cssDetails =
  fromList
    [ "border-2",
      "border",
      "border-gray-200",
      "p-4",
      "[&_svg]:open:-rotate-180]",
      "my-4",
      "rounded-2xl"
    ]

cssSummary :: Set Text
cssSummary = fromList ["text-xl", "font-bold", "cursor-pointer"]
