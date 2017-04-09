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
type HtmlAttribute = (HtmlAttributeName, HtmlAttributeValues)
type HtmlTagName = TagName
newtype HtmlAction = HtmlAction (Event -> IO ())

data Html =
     Html (Tag HtmlAttribute HtmlContent)
   | HtmlSiblings [Html]
   deriving (Show, Eq)

data HtmlContent =
     Plain String
   -- ^ Ordinary text content.
   | BreakingSpace
   -- ^ Breaking space content.
   | Style [Css.Css]
   -- ^ Style content.
   | Interpolation ExpQ
   -- ^ Interpolated content that contains Haskell expression.

data HtmlAttributeValue =
     Value String
   -- ^ Ordinary attribute value.
   | StyleValue Css.CssProperty
   -- ^ Style value.
   | EventValue HtmlAction
   -- ^ Event value that contains an action.
   deriving (Show, Eq)

data HtmlAttributeValues =
      AttributeValues [HtmlAttributeValue]
   -- ^ Ordinary attribute values.
   | InterpolationAttribute ExpQ
   -- ^ Interpolated attribute that contains Haskell expression.

class IsHtml a where
   formatHtml :: a -> Html

class IsHtmlContent a where
   formatHtmlContent :: a -> HtmlContent

class IsHtmlAttribute a where
   formatHtmlAttribute :: a -> HtmlAttributeValues

class IsHtmlAttributeValue a where
   formatHtmlAttributeValue :: a -> HtmlAttributeValue

instance Monoid HtmlContent where
   mempty =
      Plain mempty

   mappend (Plain lhs) (Plain rhs) =
      Plain $ lhs `mappend` rhs

   mappend (Style lhs) (Style rhs) =
      Style $ lhs `mappend` rhs

   mappend BreakingSpace BreakingSpace =
      BreakingSpace

   mappend (Plain "") rhs =
      rhs

   mappend lhs (Plain "") =
      lhs

   mappend lhs (Interpolation rhs) =
      let lhs' = lift lhs
          rhs' = appE [| formatHtmlContent |] rhs
      in Interpolation $ appE (appE [| mappend |] lhs') rhs'

   mappend (Interpolation lhs) rhs =
      let lhs' = appE [| formatHtmlContent |] lhs
          rhs' = lift rhs
      in Interpolation $ appE (appE [| mappend |] lhs') rhs'

   mappend lhs rhs =
      error $ "Incompatible constructs for content: " ++ show lhs ++ " and " ++ show rhs

instance Monoid HtmlAttributeValues where
   mempty =
      AttributeValues []

   -- Merge values inside attributes.
   mappend (AttributeValues (x@Value{} : xs)) (AttributeValues (y@Value{} : ys)) =
                AttributeValues [x `mappend` y]
      `mappend` AttributeValues xs
      `mappend` AttributeValues ys

   -- Join event actions inside attributes.
   mappend (AttributeValues (x@EventValue{} : xs)) (AttributeValues (y@EventValue{} : ys)) =
                AttributeValues [x `mappend` y]
      `mappend` AttributeValues xs
      `mappend` AttributeValues ys

   -- Do a high level attribute merge.
   mappend (AttributeValues lhs) (AttributeValues rhs) =
      AttributeValues $ lhs `mappend` rhs

   mappend (InterpolationAttribute lhs) (InterpolationAttribute rhs) =
      let lhs' = appE [| formatHtmlAttribute |] lhs
          rhs' = appE [| formatHtmlAttribute |] rhs
      in InterpolationAttribute $ appE (appE [| mappend |] lhs') rhs'

   mappend lhs@AttributeValues{} (InterpolationAttribute rhs) =
      let lhs' = lift lhs
          rhs' = appE [| formatHtmlAttribute |] rhs
      in InterpolationAttribute $ appE (appE [| mappend |] lhs') rhs'

   mappend (InterpolationAttribute lhs) rhs@AttributeValues{} =
      let lhs' = appE [| formatHtmlAttribute |] lhs
          rhs' = lift rhs
      in InterpolationAttribute $ appE (appE [| mappend |] lhs') rhs'

instance Monoid HtmlAttributeValue where
   mempty =
      Value mempty

   mappend (Value lhs) (Value rhs) =
      Value $ lhs `mappend` rhs

   mappend (EventValue lhs) (EventValue rhs) =
      EventValue . HtmlAction $ \event ->
         do { runAction lhs event; runAction rhs event }

   mappend (Value "") rhs =
      rhs

   mappend lhs (Value "") =
      lhs

   mappend lhs rhs =
      error $ "Incompatible constructs for attribute: " ++ show lhs ++ " and " ++ show rhs


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


instance IsHtmlAttributeValue HtmlAttributeValue where
   formatHtmlAttributeValue = id

instance IsHtmlAttributeValue Bool where
   formatHtmlAttributeValue = formatHtmlAttributeValue . show

instance IsHtmlAttributeValue Double where
   formatHtmlAttributeValue = formatHtmlAttributeValue . show

instance IsHtmlAttributeValue Float where
   formatHtmlAttributeValue = formatHtmlAttributeValue . show

instance IsHtmlAttributeValue Int where
   formatHtmlAttributeValue = formatHtmlAttributeValue . show

instance IsHtmlAttributeValue Char where
   formatHtmlAttributeValue '&' = Value "&amp;"
   formatHtmlAttributeValue '"' = Value "&quot;"
   formatHtmlAttributeValue '\'' = Value "&apos;"
   formatHtmlAttributeValue value = Value [value]

instance IsHtmlAttributeValue Text where
   formatHtmlAttributeValue = formatHtmlAttributeValue . unpack

instance IsHtmlAttributeValue ByteString where
   formatHtmlAttributeValue = formatHtmlAttributeValue . decodeUtf8

instance IsHtmlAttributeValue HtmlAction where
   formatHtmlAttributeValue = formatHtmlAttributeValue . EventValue

instance IsHtmlAttributeValue Css.CssProperty where
   formatHtmlAttributeValue = formatHtmlAttributeValue . StyleValue

instance IsHtmlAttributeValue a => IsHtmlAttributeValue [a] where
   formatHtmlAttributeValue = mconcat . map formatHtmlAttributeValue


instance IsHtmlAttribute HtmlAttributeValues where
   formatHtmlAttribute = id

instance IsHtmlAttribute HtmlAttributeValue where
   formatHtmlAttribute = AttributeValues . pure

instance IsHtmlAttribute Bool where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Double where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Float where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Int where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Char where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Text where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute ByteString where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute HtmlAction where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute Css.CssProperty where
   formatHtmlAttribute = formatHtmlAttribute . formatHtmlAttributeValue

instance IsHtmlAttribute a => IsHtmlAttribute [a] where
   formatHtmlAttribute = mconcat . map formatHtmlAttribute


instance Lift Html where
   lift (Html tag) =
      [| Html tag |]

   lift (HtmlSiblings tags) =
      [| HtmlSiblings tags |]

instance Lift HtmlAttributeValue where
   -- This comes directly from parser, no need to format.
   lift (Value content) =
      [| Value content |]

   lift (StyleValue content) =
      [| StyleValue content |]

   lift (EventValue _) =
      error "EventValues cannot be lifted"

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

instance Lift HtmlAttributeValues where
   -- This comes directly from parser, no need to format.
   lift (AttributeValues values) =
      [| AttributeValues values |]

   -- This is evaluated Haskell syntax that parser has not seen, need to format.
   lift (InterpolationAttribute expression) =
      appE [| formatHtmlAttribute |] expression


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

instance Show HtmlAttributeValues where
   show (AttributeValues values) =
      show values

   show (InterpolationAttribute _) =
      "${..}"

instance Eq HtmlAttributeValues where
   (==) (AttributeValues lhs) (AttributeValues rhs) =
      lhs == rhs

   (==) (InterpolationAttribute _) (InterpolationAttribute _) =
      True

   (==) _ _ =
      False

instance Show HtmlAction where
   show HtmlAction{} =
      "{action}"

instance Eq HtmlAction where
   (==) HtmlAction{} HtmlAction{} =
      True


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
