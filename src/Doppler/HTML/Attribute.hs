{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.HTML.Attribute (
   Attribute (..), Key, Value (..), Collection (..), IsHTMLAttribute (..),
   Action (..)
) where

import Doppler.CSS.Types          (Property)
import Doppler.Event.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type Key = String
newtype Action = Action (Event -> IO ())

data Value =
     StringValue String
   | InterpolationValue String
   | CSSValue Property
   | EventValue Action

data Collection =
     Values [Value]
   | Interpolation String
   deriving (Eq, Show)

newtype Attribute = Attribute {
   getAttribute :: (Key, Collection)
} deriving (Eq, Show)

class IsHTMLAttribute a where
   toAttributeValue :: a -> Value

instance Monoid Value where
   mempty =
      StringValue ""

   mappend (StringValue a) (StringValue b) =
      StringValue (a ++ b)

   mappend a _ =
      a

instance IsHTMLAttribute Value where
   toAttributeValue = id

instance IsHTMLAttribute Bool where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Char where
   toAttributeValue = StringValue . pure

instance IsHTMLAttribute Double where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Float where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Int where
   toAttributeValue = StringValue . show

instance IsHTMLAttribute Property where
   toAttributeValue = CSSValue

instance IsHTMLAttribute Action where
   toAttributeValue = EventValue

instance IsHTMLAttribute a => IsHTMLAttribute [a] where
   toAttributeValue =
      mconcat . map toAttributeValue

instance Lift Value where
   lift (StringValue content) =
      [|StringValue content|]

   lift (InterpolationValue content) =
      appE [|StringValue|] $ runInterpolation content

   lift (CSSValue props) =
      [|CSSValue props|]

   lift (EventValue _) =
      error "Internal error, event values cannot be lifted"

instance Lift Collection where
   lift (Values values) =
      [|Values values|]

   lift (Interpolation content) =
      appE [|Values . map toAttributeValue|] $ runInterpolation content

instance Lift Attribute where
   lift (Attribute attr) =
      [|Attribute attr|]

instance Eq Value where
   (==) (StringValue lhs) (StringValue rhs) =
      lhs == rhs

   (==) (InterpolationValue lhs) (InterpolationValue rhs) =
      lhs == rhs

   (==) (CSSValue lhs) (CSSValue rhs) =
      lhs == rhs

   (==) (EventValue _) (EventValue _) =
      True

   (==) _ _ =
      False

instance Show Value where
   show (StringValue value) =
      "StringValue " ++ value

   show (InterpolationValue value) =
      "InterpolationValue " ++ value

   show (CSSValue value) =
      "CSSValue " ++ show value

   show (EventValue _) =
      "EventValue"

runInterpolation :: String -> Q Exp
runInterpolation =
   foldl1 appE . map (varE . mkName) . words
