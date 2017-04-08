{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Html.Types (
   module Doppler.Html.Types,
   module Doppler.Tag.Types
) where

import Doppler.Tag.Types
import Doppler.Event.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Doppler.Css.Types  as Css
import Data.Text                    (Text, unpack)
import Data.Text.Encoding           (decodeUtf8)
import Data.ByteString              (ByteString)

type HtmlAttributeName = String
type HtmlAttribute = (HtmlAttributeName, [HtmlAttributeValue])
type HtmlTagName = TagName
data Html =
     Html (Tag HtmlAttribute HtmlContent)
   | HtmlSiblings [Html]
   deriving (Show, Eq)
newtype HtmlAction = HtmlAction (Event -> IO ())

data HtmlContent =
     Plain String
   -- ^ Ordinary text content.
   | BreakingSpace
   -- ^ Breaking space content.
   | Style [Css.Css]
   -- ^ Style content.
   | Interpolation (Q Exp)
   -- ^ Interpolated content that contains Haskell expression.

data HtmlAttributeValue =
     Value String
   -- ^ Ordinary attribute value.
   | StyleValue Css.CssProperty
   -- ^ Style value.
   | EventValue HtmlAction
   -- ^ Event value that contains an action.
   | InterpolationValue (Q Exp)
   -- ^ Interpolated value that contains Haskell expression.

class IsHtml a where
   formatHtml :: a -> Html

class IsHtmlContent a where
   formatHtmlContent :: a -> HtmlContent

class IsHtmlAttribute a where
   formatAttribute :: a -> HtmlAttributeValue


instance Monoid HtmlContent where
   mempty =
      Plain mempty

   mappend (Plain lhs) (Plain rhs) =
      Plain $ lhs `mappend` rhs

   mappend (Style lhs) (Style rhs) =
      Style $ lhs `mappend` rhs

   mappend BreakingSpace BreakingSpace =
      BreakingSpace

   mappend lhs@(Interpolation _) (Interpolation _) =
      Plain $ show lhs

   mappend lhs rhs =
      Plain $ show lhs `mappend` show rhs


instance Monoid HtmlAttributeValue where
   mempty =
      Value mempty

   mappend (Value lhs) (Value rhs) =
      Value $ lhs `mappend` rhs

   mappend (StyleValue (Css.CssProperty (lhsName, lhsProps))) (StyleValue (Css.CssProperty (rhsName, rhsProps)))
      | lhsName == rhsName =
         StyleValue $ Css.CssProperty (lhsName, lhsProps `mappend` rhsProps)
      | otherwise =
         StyleValue $ Css.CssProperty (lhsName, lhsProps)

   mappend (Value lhs) (StyleValue rhs) =
      Value $ lhs `mappend` show rhs

   mappend (EventValue lhs) (EventValue rhs) =
      EventValue . HtmlAction $ \event ->
         do { runAction lhs event; runAction rhs event }

   mappend (StyleValue lhs) (Value rhs) =
      Value $ show lhs `mappend` rhs

   mappend lhs@(InterpolationValue _) (InterpolationValue _) =
      Value $ show lhs

   mappend lhs rhs =
      Value $ show lhs `mappend` show rhs


instance TagContent HtmlContent where
   liftContent (Interpolation expression) =
      appE [| getSiblings . formatHtml |] expression

   liftContent content =
      [| [Content content] |]


instance IsHtml Html where
   formatHtml = id

instance IsHtml HtmlContent where
   formatHtml = Html . Content

instance IsHtml Bool where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml Double where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml Float where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml Int where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml Char where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml Text where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml ByteString where
   formatHtml = Html . Content . formatHtmlContent

instance IsHtml a => IsHtml [a] where
   formatHtml = HtmlSiblings . mconcatContent . map formatHtml


instance IsHtmlContent HtmlContent where
   formatHtmlContent = id

instance IsHtmlContent Bool where
   formatHtmlContent = formatHtmlContent . show

instance IsHtmlContent Double where
   formatHtmlContent = formatHtmlContent . show

instance IsHtmlContent Float where
   formatHtmlContent = formatHtmlContent . show

instance IsHtmlContent Int where
   formatHtmlContent = formatHtmlContent . show

instance IsHtmlContent Char where
   formatHtmlContent '<' = Plain "&lt;"
   formatHtmlContent '>' = Plain "&gt;"
   formatHtmlContent '&' = Plain "&amp;"
   formatHtmlContent value = Plain [value]

instance IsHtmlContent Text where
   formatHtmlContent = formatHtmlContent . unpack

instance IsHtmlContent ByteString where
   formatHtmlContent = formatHtmlContent . decodeUtf8

instance IsHtmlContent a => IsHtmlContent [a] where
   formatHtmlContent = mconcat . map formatHtmlContent


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

instance IsHtmlAttribute HtmlAction where
   formatAttribute = EventValue

instance IsHtmlAttribute Css.CssProperty where
   formatAttribute = StyleValue

instance IsHtmlAttribute a => IsHtmlAttribute [a] where
   formatAttribute = mconcat . map formatAttribute


instance Lift Html where
   lift (Html tag) =
      [| Html tag |]


instance Show HtmlContent where
   show (Plain content) =
      show content

   show BreakingSpace =
      "BreakingSpace"

   show (Style content) =
      show content

   show (Interpolation _) =
      "${..}"

instance Eq HtmlContent where
   (==) (Plain lhs) (Plain rhs) =
      lhs == rhs

   (==) BreakingSpace BreakingSpace =
      True

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

   lift BreakingSpace =
      [| BreakingSpace |]

   lift (Style content) =
      [| Style content |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (Interpolation expression) =
      appE [| formatHtmlContent |] expression


instance Show HtmlAttributeValue where
   show (Value content) =
      show content

   show (EventValue _) =
      "{event}"

   show (InterpolationValue _) =
      "${..}"

instance Eq HtmlAttributeValue where
   (==) (Value lhs) (Value rhs) =
      lhs == rhs

   (==) (StyleValue lhs) (StyleValue rhs) =
      lhs == rhs

   (==) (EventValue _) (EventValue _) =
      True

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

   lift (EventValue _) =
      error "EventValues cannot be lifted"

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (InterpolationValue expression) =
      appE [| formatAttribute |] expression


runAction :: HtmlAction -> Event -> IO ()
runAction (HtmlAction action) =
   action

mconcatContent :: [Html] -> [Html]
mconcatContent (Html (Content a) : Html (Content b) : xs) =
    mconcatContent $ Html (Content $ mappend a b) : xs

mconcatContent (x:xs) =
   x : mconcatContent xs

mconcatContent [] =
   []

getHtml :: Html -> Tag HtmlAttribute HtmlContent
getHtml (Html tag) =
   tag

getHtml _ =
   error "HtmlSiblings is not a valid HTML root"

getSiblings :: Html -> [Tag HtmlAttribute HtmlContent]
getSiblings (Html tag) =
   pure tag

getSiblings (HtmlSiblings siblings) =
   concatMap getSiblings siblings
