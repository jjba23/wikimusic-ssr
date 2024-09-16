{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module WikiMusic.SSR.View.Css where

import Data.Text qualified as T
import Optics
import Relude
import Text.Blaze.Html qualified as BlazeHtml
import Text.Blaze.Html5.Attributes qualified as A
import WikiMusic.SSR.Model.Api

newtype Css = Css {className :: Text} deriving (Generic, Eq, Show)

makeFieldLabelsNoPrefix ''Css

cssToAttrValue :: [Text] -> BlazeHtml.AttributeValue
cssToAttrValue = fromString . T.unpack . joinClasses . fromList
  where
    joinClasses :: Set Text -> Text
    joinClasses = T.intercalate " " . sort . toList

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
      "font-sans",
      "w-fit"
    ]

cssButton :: ViewVars -> Set Text
cssButton vv =
  fromList
    [ if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black",
      "hover:bg-" <> vv ^. #palette % #value <> "-500/40",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-gray-300",
      "font-medium",
      "rounded-2xl",
      "text-md",
      "px-5",
      "py-2.5",
      "font-sans",
      "w-fit",
      "border",
      "border-" <> vv ^. #palette % #value <> "-500/40",
      "bg-" <> vv ^. #palette % #value <> "-400/40"
    ]

cssSelect :: ViewVars -> Set Text
cssSelect vv =
  fromList
    [ if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black",
      "bg-" <> vv ^. #palette % #value <> "-400/40",
      "hover:bg-" <> vv ^. #palette % #value <> "-500/40",
      "focus:outline-none",
      "focus:ring-4",
      "focus:ring-gray-300",
      "font-medium",
      "rounded-2xl",
      "text-sm",
      "px-8",
      "cursor-pointer",
      "py-2",
      "font-sans",
      "w-fit",
      "h-fit",
      "border-" <> vv ^. #palette % #value <> "-400/40"
    ]

cssCenteredCardGrid :: Set Text
cssCenteredCardGrid = fromList ["flex", "flex-row", "flex-wrap", "gap-4", "justify-center", "align-center"]

cssInput :: Set Text
cssInput = fromList ["rounded-2xl", "px-5", "py-2.5", "font-sans", "w-full", "bg-white/40"]

cssTextarea :: Set Text
cssTextarea = fromList ["rounded-2xl", "px-5", "py-2.5", "w-full", "h-fit", "min-h-72", "bg-white/40"]

cssDetails :: ViewVars -> Set Text
cssDetails vv =
  fromList
    [ "border-2",
      "border",
      "border-gray-200",
      "p-4",
      "[&_svg]:open:-rotate-180]",
      "my-4",
      "rounded-2xl",
      if vv ^. #uiMode % #value == "dark" then "bg-black/70" else "bg-white/80",
      if vv ^. #uiMode % #value == "dark" then "text-white" else "text-black"
    ]

cssSummary :: Set Text
cssSummary = fromList ["text-xl", "font-bold", "cursor-pointer"]

cssLink :: ViewVars -> Set Text
cssLink vv = fromList ["text-" <> vv ^. #palette % #value <> "-500", "font-bold", "cursor-pointer"]
