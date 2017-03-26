{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Html.Types (
   module Doppler.Html.Types,
   module Doppler.Tag.Types
) where

import Doppler.Tag.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type HtmlAttributeName = String
type HtmlAttribute = (HtmlAttributeName, [HtmlAttributeValue])
type HtmlTagName = TagName
type Html = Tag HtmlAttribute HtmlContent

data HtmlContent =
     Text String
   -- ^ Ordinary text content.
   | Interpolation (Q Exp)
   -- ^ Interpolated content that contains Haskell expression.

data HtmlAttributeValue =
     Value String
   -- ^ Ordinary attribute value.
   | InterpolationValue (Q Exp)
   -- ^ Interpolated value that contains Haskell expression.

class IsHtmlContent a where
   formatHtml :: a -> String

class IsHtmlAttribute a where
   formatAttribute :: a -> String


instance IsHtmlContent Bool where
   formatHtml = formatHtml . show

instance IsHtmlContent Double where
   formatHtml = formatHtml . show

instance IsHtmlContent Float where
   formatHtml = formatHtml . show

instance IsHtmlContent Int where
   formatHtml = formatHtml . show

instance IsHtmlContent Char where
   formatHtml '<' = "&lt;"
   formatHtml '>' = "&gt;"
   formatHtml '&' = "&amp;"
   formatHtml value = pure value

instance IsHtmlContent a => IsHtmlContent [a] where
   formatHtml = mconcat . map formatHtml


instance IsHtmlAttribute Bool where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Double where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Float where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Int where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Char where
   formatAttribute '&' = "&amp;"
   formatAttribute '"' = "&quot;"
   formatAttribute '\'' = "&apos;"
   formatAttribute value = pure value

instance IsHtmlAttribute a => IsHtmlAttribute [a] where
   formatAttribute = mconcat . map formatAttribute


instance Show HtmlContent where
   show (Text content) =
      show content

   show (Interpolation _) =
      "${..}"

instance Eq HtmlContent where
   (==) (Text lhs) (Text rhs) =
      lhs == rhs

   (==) (Interpolation _) (Interpolation _) =
      True

   (==) _ _ =
      False

instance Lift HtmlContent where
   -- This comes directly from parser, no need to format.
   lift (Text content) =
      [| Text content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (Interpolation expression) =
      appE [| Text . formatHtml |] expression


instance Show HtmlAttributeValue where
   show (Value content) =
      show content

   show (InterpolationValue _) =
      "${..}"

instance Eq HtmlAttributeValue where
   (==) (Value lhs) (Value rhs) =
      lhs == rhs

   (==) (InterpolationValue _) (InterpolationValue _) =
      True

   (==) _ _ =
      False

instance Lift HtmlAttributeValue where
   -- This comes directly from parser, no need to format.
   lift (Value content) =
      [| Value content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (InterpolationValue expression) =
      appE [| Value . formatAttribute |] expression
