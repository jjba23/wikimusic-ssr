{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.View.Classes.Classes where

import Data.Text qualified as T
import Optics
import Relude
import Text.Blaze.Html qualified as BlazeHtml

newtype Class = Class {value :: Text} deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''Class

joinClasses :: Set Text -> Text
joinClasses = T.intercalate " " . toList

splitClasses :: Text -> Set Text
splitClasses = fromList . T.splitOn " "

classListToAttributeValue :: [Text] -> BlazeHtml.AttributeValue
classListToAttributeValue = fromString . T.unpack . joinClasses . fromList

(@@) :: [Text] -> BlazeHtml.AttributeValue
(@@) = classListToAttributeValue

submitButtonClass :: Text
submitButtonClass =
  joinClasses
    . fromList
    $ [ "text-white",
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

selectClass :: Text
selectClass =
  joinClasses
    . fromList
    $ [ "block",
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

someButtonClass :: Text
someButtonClass =
  joinClasses
    . fromList
    $ [ "text-black",
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

entityCardSectionClass :: Text
entityCardSectionClass = joinClasses . fromList $ ["flex", "flex-row", "flex-wrap", "gap-4"]