{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.HTML.Expression (
   Expression (Element, Text, Include), IsHTML (..)
) where

import Doppler.HTML.Attribute
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Text                  (Text, unpack)
import Data.Text.Encoding         (decodeUtf8)
import Data.ByteString            (ByteString)

data Expression =
     Element String [Attribute] [Expression]
   | Text String
   | Include String
   | ChildContext [Expression]
   deriving (Eq, Show)

class IsHTML a where
   toExpression :: a -> Expression

instance IsHTML Expression where
   toExpression = id

instance IsHTML Bool where
   toExpression = Text . show

instance IsHTML Char where
   toExpression = Text . pure

instance IsHTML Double where
   toExpression = Text . show

instance IsHTML Float where
   toExpression = Text . show

instance IsHTML Int where
   toExpression = Text . show

instance IsHTML Text where
   toExpression = Text . unpack

instance IsHTML ByteString where
   toExpression = toExpression . decodeUtf8

instance IsHTML a => IsHTML [a] where
   toExpression =
      ChildContext . map toExpression

instance Lift Expression where
   lift (Element tagName attrs childs) =
      [|Element tagName attrs $ expandChildren childs|]

   lift (Text content) =
      [|Text content|]

   lift (Include content) =
      appE [|toExpression|] $ runInterpolation content

   lift (ChildContext _) =
      error "Internal error, child context should be expanded"


expandChildren :: [Expression] -> [Expression]
expandChildren =
   foldr expandChildren' []
   where
      expandChildren' expression acc =
         case expression of
            ChildContext childs -> expandChildren childs ++ acc
            _ -> expression : acc

runInterpolation :: String -> Q Exp
runInterpolation =
   foldl1 appE . map (varE . mkName) . words
