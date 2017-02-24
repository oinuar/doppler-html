module Doppler.HTML.SyntaxSpec where

import Test.Hspec
import Doppler.HTML.Types
import Doppler.HTML.Syntax
import Control.Exception           (evaluate)
import qualified Doppler.CSS.Types as CSS


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "Tags" $ do
      it "should work with element without children" $
         parseFromString "<foo></foo>" `shouldBe`
            Element "FOO" [] []

      it "should work with element with one child" $
         parseFromString "<foo><span></span></foo>" `shouldBe`
            Element "FOO" [] [Element "SPAN" [] []]

      it "should work with element with two childs" $
         parseFromString "<foo><span></span><div></div></foo>" `shouldBe`
            Element "FOO" [] [Element "SPAN" [] [], Element "DIV" [] []]

      it "should work with element with two childs of childs" $
         parseFromString "<foo><span><bar></bar></span><div></div></foo>" `shouldBe`
            Element "FOO" [] [Element "SPAN" [] [Element "BAR" [] []], Element "DIV" [] []]

      it "should fail if tags are closed in wrong order" $
         evaluate (parseFromString "<foo><span></foo></span>") `shouldThrow`
            anyErrorCall

      it "should fail if tag is not closed" $
         evaluate (parseFromString "<foo>") `shouldThrow`
            anyErrorCall

      it "should work with short-closes" $
         parseFromString "<foo/>" `shouldBe`
            Element "FOO" [] []

      it "should work with short-closes with space" $
         parseFromString "<foo />" `shouldBe`
            Element "FOO" [] []

   describe "Text nodes" $ do
      it "should allow text as child nodes" $
         parseFromString "<foo>bar</foo>" `shouldBe`
            Element "FOO" [] [Text "bar"]

      it "should not allow text as root element" $
         evaluate (parseFromString "bar") `shouldThrow`
            anyErrorCall

      it "should allow mixing text and other child nodes" $
         parseFromString "<foo>bar<div /></foo>" `shouldBe`
            Element "FOO" [] [Text "bar", Element "DIV" [] []]

   describe "Includes" $ do
      it "should allow includes" $
         parseFromString "<foo>${include}</foo>" `shouldBe`
            Element "FOO" [] [Include "include"]

      it "should allow multiple includes" $
         parseFromString "<foo>${include}${x}</foo>" `shouldBe`
            Element "FOO" [] [Include "include", Include "x"]

      it "should allow multiple includes separated by space" $
         parseFromString "<foo>${include} ${x}</foo>" `shouldBe`
            Element "FOO" [] [Include "include", Text " ", Include "x"]

      it "should allow include within text" $
         parseFromString "<foo>Hello ${name}, how are you?</foo>" `shouldBe`
            Element "FOO" [] [Text "Hello ", Include "name", Text ", how are you?"]

      it "should not allow root level include" $
         evaluate (parseFromString "${include}") `shouldThrow`
            anyErrorCall

   describe "Attributes" $ do
      it "should allow string attribute" $
         parseFromString "<foo attr=\"value\"></foo>" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "value"])] []

      it "should allow short-close string attribute" $
         parseFromString "<foo attr=\"value\" />" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "value"])] []

      it "should allow string attribute element with children" $
         parseFromString "<foo attr=\"value\"><span /></foo>" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "value"])] [Element "SPAN" [] []]

      it "should allow multiple short-close attributes" $
         parseFromString "<foo attr=\"value\" key=\"val\" />" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "value"]), Attribute ("key", Values [StringValue "val"])] []

      it "should allow child element with attribute" $
         parseFromString "<foo attr=\"value\"><span key=\"val\" /></foo>" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "value"])] [Element "SPAN" [Attribute ("key", Values [StringValue "val"])] []]

      it "should allow string interpolation" $
         parseFromString "<foo attr=\"Hello ${jaska}.\" />" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [StringValue "Hello ", InterpolationValue "jaska", StringValue "."])] []

      it "should allow single string interpolation" $
         parseFromString "<foo attr=\"${n}\" />" `shouldBe`
            Element "FOO" [Attribute ("attr", Values [InterpolationValue "n"])] []

      it "should allow interpolation" $
         parseFromString "<foo attr=${n} />" `shouldBe`
            Element "FOO" [Attribute ("attr", Interpolation "n")] []

   describe "CSS style-attribute" $ do
      it "should allow property" $
         parseFromString "<foo style=\"text-align: center\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("text-align", [CSS.StringValue "center"])])] []

      it "should allow property with semi-colon" $
         parseFromString "<foo style=\"text-align: center;\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("text-align", [CSS.StringValue "center"])])] []

      it "should allow property with spaces" $
         parseFromString "<foo style=\"border: 1px solid black\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("border", [CSS.StringValue "1px solid black"])])] []

      it "should allow property with precent unit" $
         parseFromString "<foo style=\"font-size: 100%\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("font-size", [CSS.StringValue "100%"])])] []

      it "should allow property with hex color" $
         parseFromString "<foo style=\"color: #000\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("color", [CSS.StringValue "#000"])])] []

      it "should allow property with rgb color" $
         parseFromString "<foo style=\"color: rgb(0, 0, 0)\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("color", [CSS.StringValue "rgb(0, 0, 0)"])])] []

      it "should allow properties with semi-colons" $
         parseFromString "<foo style=\"text-align: center; font-size: 17px;\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("text-align", [CSS.StringValue "center"]), CSSValue $ CSS.Property ("font-size", [CSS.StringValue "17px"])])] []

      it "should allow interpolation" $
         parseFromString "<foo style=\"font-size: ${size}px\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("font-size", [CSS.InterpolationValue "size", CSS.StringValue "px"])])] []

      it "should allow single interpolation" $
         parseFromString "<foo style=\"text-align: ${align}\" />" `shouldBe`
            Element "FOO" [Attribute ("style", Values [CSSValue $ CSS.Property ("text-align", [CSS.InterpolationValue "align"])])] []
