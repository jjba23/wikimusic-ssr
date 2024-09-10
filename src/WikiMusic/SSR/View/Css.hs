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

newtype CSS = CSS {className :: Text} deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''CSS

classListToAttributeValue :: [Text] -> BlazeHtml.AttributeValue
classListToAttributeValue = fromString . T.unpack . joinClasses . fromList
  where
    joinClasses :: Set Text -> Text
    joinClasses = T.intercalate " " . toList

css' :: [Text] -> BlazeHtml.Attribute
css' xs = css (fromList xs :: Set Text)

css :: Set Text -> BlazeHtml.Attribute
css xs = A.class_ (classListToAttributeValue $ toList xs)

submitButtonClass :: Set Text
submitButtonClass =
  fromList
    [ "text-white",
      "bg-green-700",
      "hover:bg-green-800",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-green-300",
      "font-medium",
      "rounded-full",
      "text-sm",
      "px-5",
      "py-2.5",
      "text-center",
      "me-2",
      "mb-2",
      "font-sans",
      "w-fit"
    ]

selectClass :: Set Text
selectClass =
  fromList
    [ "block",
      "w-fit",
      "p-4",
      "text-gray-900",
      "border",
      "border-gray-300",
      "rounded-lg",
      "bg-gray-100",
      "text-base",
      "focus:ring-blue-500",
      "focus:border-blue-500",
      "font-sans"
    ]

someButtonClass :: Set Text
someButtonClass =
  fromList
    [ "text-black",
      "bg-gray-200",
      "hover:bg-gray-300",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-gray-300",
      "font-medium",
      "rounded-full",
      "text-lg",
      "px-5",
      "py-2.5",
      "me-2",
      "mb-2",
      "font-sans",
      "w-fit"
    ]

entityCardSectionClass :: Set Text
entityCardSectionClass = fromList ["flex", "flex-row", "flex-wrap", "gap-4"]

inputClass :: Set Text
inputClass = fromList ["rounded-xl", "px-5", "py-2.5", "font-sans"]

textareaClass :: Set Text
textareaClass = fromList ["rounded-xl", "px-5", "py-2.5"]
