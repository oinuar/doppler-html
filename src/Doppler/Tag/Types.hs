{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Doppler.Tag.Types (
   TagName, Tag (..), Quote (..), getTagName
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

instance (Lift a, Lift b) => Lift (Tag a b) where
   lift (FullTag name attributes children) =
      [| FullTag name attributes children |]

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
   error "Content has no tag name"

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
