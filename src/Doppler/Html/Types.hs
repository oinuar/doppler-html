{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Html.Types (
   module Doppler.Html.Types,
   module Doppler.Tag.Types
) where

import Doppler.Tag.Types
import qualified Doppler.Css.Types as Css
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Text                      (Text, unpack)
import Data.Text.Encoding             (decodeUtf8)
import Data.ByteString                (ByteString)

type HtmlAttributeName = String
type HtmlAttribute = (HtmlAttributeName, [HtmlAttributeValue])
type HtmlTagName = TagName
type Html = Tag HtmlAttribute HtmlContent

data HtmlContent =
     Plain String
   -- ^ Ordinary text content.
   | Style [Css.Css]
   -- ^ Style content.
   | Interpolation (Q Exp)
   -- ^ Interpolated content that contains Haskell expression.

data HtmlAttributeValue =
     Value String
   -- ^ Ordinary attribute value.
   | StyleValue Css.CssProperty
   -- ^ Style value.
   | InterpolationValue (Q Exp)
   -- ^ Interpolated value that contains Haskell expression.

class IsHtmlContent a where
   formatHtml :: a -> HtmlContent

class IsHtmlAttribute a where
   formatAttribute :: a -> HtmlAttributeValue


instance Monoid HtmlContent where
   mempty =
      Plain mempty

   mappend (Plain lhs) (Plain rhs) =
      Plain $ lhs `mappend` rhs

   mappend (Style lhs) (Style rhs) =
      Style $ lhs `mappend` rhs

   mappend lhs@(Interpolation _) (Interpolation _) =
      Plain $ show lhs

   mappend lhs rhs =
      Plain $ show lhs `mappend` show rhs


instance Monoid HtmlAttributeValue where
   mempty =
      Value mempty

   mappend (Value lhs) (Value rhs) =
      Value $ lhs `mappend` rhs

   mappend (StyleValue (lhsName, lhsProps)) (StyleValue (rhsName, rhsProps))
      | lhsName == rhsName =
         StyleValue (lhsName, lhsProps `mappend` rhsProps)
      | otherwise =
         StyleValue (lhsName, lhsProps)

   mappend (Value lhs) (StyleValue rhs) =
      Value $ lhs `mappend` show rhs

   mappend (StyleValue lhs) (Value rhs) =
      Value $ show lhs `mappend` rhs

   mappend lhs@(InterpolationValue _) (InterpolationValue _) =
      Value $ show lhs

   mappend lhs rhs =
      Value $ show lhs `mappend` show rhs


instance IsHtmlContent HtmlContent where
   formatHtml = id

instance IsHtmlContent Bool where
   formatHtml = formatHtml . show

instance IsHtmlContent Double where
   formatHtml = formatHtml . show

instance IsHtmlContent Float where
   formatHtml = formatHtml . show

instance IsHtmlContent Int where
   formatHtml = formatHtml . show

instance IsHtmlContent Char where
   formatHtml '<' = Plain "&lt;"
   formatHtml '>' = Plain "&gt;"
   formatHtml '&' = Plain "&amp;"
   formatHtml value = Plain [value]

instance IsHtmlContent Text where
   formatHtml = formatHtml . unpack

instance IsHtmlContent ByteString where
   formatHtml = formatHtml . decodeUtf8

instance IsHtmlContent a => IsHtmlContent [a] where
   formatHtml = mconcat . map formatHtml


instance IsHtmlAttribute HtmlAttributeValue where
   formatAttribute = id

instance IsHtmlAttribute Bool where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Double where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Float where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Int where
   formatAttribute = formatAttribute . show

instance IsHtmlAttribute Char where
   formatAttribute '&' = Value "&amp;"
   formatAttribute '"' = Value "&quot;"
   formatAttribute '\'' = Value "&apos;"
   formatAttribute value = Value [value]

instance IsHtmlAttribute Text where
   formatAttribute = formatAttribute . unpack

instance IsHtmlAttribute ByteString where
   formatAttribute = formatAttribute . decodeUtf8

instance IsHtmlAttribute a => IsHtmlAttribute [a] where
   formatAttribute = mconcat . map formatAttribute


instance Show HtmlContent where
   show (Plain content) =
      show content

   show (Style content) =
      show content

   show (Interpolation _) =
      "${..}"

instance Eq HtmlContent where
   (==) (Plain lhs) (Plain rhs) =
      lhs == rhs

   (==) (Style lhs) (Style rhs) =
      lhs == rhs

   (==) (Interpolation _) (Interpolation _) =
      True

   (==) _ _ =
      False

instance Lift HtmlContent where
   -- This comes directly from parser, no need to format.
   lift (Plain content) =
      [| Plain content |]

   lift (Style content) =
      [| Style content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (Interpolation expression) =
      appE [| formatHtml |] expression


instance Show HtmlAttributeValue where
   show (Value content) =
      show content

   show (InterpolationValue _) =
      "${..}"

instance Eq HtmlAttributeValue where
   (==) (Value lhs) (Value rhs) =
      lhs == rhs

   (==) (StyleValue lhs) (StyleValue rhs) =
      lhs == rhs

   (==) (InterpolationValue _) (InterpolationValue _) =
      True

   (==) _ _ =
      False

instance Lift HtmlAttributeValue where
   -- This comes directly from parser, no need to format.
   lift (Value content) =
      [| Value content |]

   lift (StyleValue content) =
      [| StyleValue content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (InterpolationValue expression) =
      appE [| formatAttribute |] expression
