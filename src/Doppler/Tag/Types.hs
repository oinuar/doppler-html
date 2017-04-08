{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Tag.Types (
   TagName, Tag (..), Quote (..), TagContent (..), getTagName
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type TagName = String

data Tag a b =
     FullTag TagName [a] [Tag a b]
   -- ^ Full tag that can contain children tags. For example, <p>content</p>.
   | DanglingTag TagName [a]
   -- ^ Dangling that that have been implicitly closed. For example, <br>.
   | ShortTag TagName [a]
   -- ^ Short tag that is explicitly closed. For example, <br />.
   | Content b
   -- ^ Tag content
   deriving (Show, Eq)

data Quote =
     Unquoted
   | SingleQuotes
   | DoubleQuotes
   deriving (Show, Eq)

class TagContent b where
   liftContent :: b -> Q Exp

instance (Lift a, Lift b, TagContent b) => Lift (Tag a b) where
   lift (FullTag name attributes children) =
      let childs = ListE <$> mapM explodeTree children
          concatChilds = appE [| concat |] childs
      in appE [| FullTag name attributes |] concatChilds

   lift (DanglingTag name attributes) =
      [| DanglingTag name attributes |]

   lift (ShortTag name attributes) =
      [| ShortTag name attributes |]

   lift (Content content) =
      [| Content content |]

-- Gets a tag name for tag.
getTagName :: Tag a b ->
              -- ^ Tag.
              TagName
              -- ^ Tag name of the tag.

getTagName (FullTag name _ _) =
   name

getTagName (DanglingTag name _) =
   name

getTagName (ShortTag name _) =
   name

getTagName (Content _) =
   mempty

-- Gets an attribute list for tag.
getAttributes :: Tag a b ->
                 -- ^ Tag.
                 [a]
                 -- ^ Attribute list for a tag.

getAttributes (FullTag _ attr _) =
   attr

getAttributes (DanglingTag _ attr) =
   attr

getAttributes (ShortTag _ attr) =
   attr

getAttributes _ =
   []

explodeTree :: (Lift a, Lift b, TagContent b) => Tag a b -> Q Exp
explodeTree (Content content) =
   liftContent content

explodeTree tag =
   appE [| (:[]) |] $ lift tag
