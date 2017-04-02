{-# LANGUAGE QuasiQuotes #-}

module Doppler.Html.SyntaxSpec where

import Test.Hspec
import Doppler.Html.Types
import qualified Doppler.Css.Types as Css
import Doppler.Html.Syntax
import Language.Haskell.TH.Syntax (lift)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "Tags" $ do
      it "parses short closed root element" $
         parseHtmlFromString "<html/>" `shouldBe`
            ShortTag "html" []

      it "parses short closed spaced root element" $
         parseHtmlFromString " <html/> " `shouldBe`
            ShortTag "html" []

      it "parses spaced short closed root element" $
         parseHtmlFromString "<html />" `shouldBe`
            ShortTag "html" []

      it "parses dangling root element" $
         parseHtmlFromString "<html>" `shouldBe`
            DanglingTag "html" []

      it "parses empty root element" $
         parseHtmlFromString "<html></html>" `shouldBe`
            FullTag "html" [] []

      it "parses element with one short closed children" $
         parseHtmlFromString "<html><br /></html>" `shouldBe`
            FullTag "html" [] [ShortTag "br" []]

      it "parses element with dangling children" $
         parseHtmlFromString "<html><br></html>" `shouldBe`
            FullTag "html" [] [DanglingTag "br" []]

      it "parses element with full children" $
         parseHtmlFromString "<html><b></b></html>" `shouldBe`
            FullTag "html" [] [FullTag "b" [] []]

      it "parses element with mixed children" $
         parseHtmlFromString "<html><br><p><br></p><br></html>" `shouldBe`
            FullTag "html" [] [DanglingTag "br" [], FullTag "p" [] [DanglingTag "br" []], DanglingTag "br" []]

   describe "Attributes" $ do
      it "parses unquoted attribute for short closed root element" $
         parseHtmlFromString "<html key=value />" `shouldBe`
            ShortTag "html" [("key", [Value "value"])]

      it "parses unquoted attribute for dangling root element" $
         parseHtmlFromString "<html key=value/>" `shouldBe`
            DanglingTag "html" [("key", [Value "value/"])]

      it "parses single quoted attribute for short closed root element" $
         parseHtmlFromString "<html key='value\"s' />" `shouldBe`
            ShortTag "html" [("key", [Value "value\"s"])]

      it "parses double quoted attribute for spaced short closed root element" $
         parseHtmlFromString "<html key=\"value's\" />" `shouldBe`
            ShortTag "html" [("key", [Value "value's"])]

      it "parses empty attribute for spaced short closed root element" $
         parseHtmlFromString "<html key />" `shouldBe`
            ShortTag "html" [("key", [])]

      it "parses multiple unquoted attribute for short closed root element" $
         parseHtmlFromString "<html key1=value1 key2=value2 />" `shouldBe`
            ShortTag "html" [("key1", [Value "value1"]), ("key2", [Value "value2"])]

      it "parses unquoted attribute for root element" $
         parseHtmlFromString "<html key=value></html>" `shouldBe`
            FullTag "html" [("key", [Value "value"])] []

      it "parses single quoted attribute for root element" $
         parseHtmlFromString "<html key='value'></html>" `shouldBe`
            FullTag "html" [("key", [Value "value"])] []

      it "parses double quoted attribute for root element" $
         parseHtmlFromString "<html key=\"value\"></html>" `shouldBe`
            FullTag "html" [("key", [Value "value"])] []

      it "parses single quoted empty attribute for root element" $
         parseHtmlFromString "<html key=''></html>" `shouldBe`
            FullTag "html" [("key", [])] []

      it "parses double quoted empty attribute for root element" $
         parseHtmlFromString "<html key=\"\"></html>" `shouldBe`
            FullTag "html" [("key", [])] []

      it "parses unquoted attribute interpolation for short closed root element" $
         parseHtmlFromString "<html key=${foo} />" `shouldBe`
            ShortTag "html" [("key", [InterpolationValue (lift "")])]

      it "parses unquoted attribute interpolation and values (post) for short closed root element" $
         parseHtmlFromString "<html key=${foo}bar />" `shouldBe`
            ShortTag "html" [("key", [InterpolationValue (lift ""), Value "bar"])]

      it "parses unquoted attribute interpolation and values (pre) for short closed root element" $
         parseHtmlFromString "<html key=bar${foo} />" `shouldBe`
            ShortTag "html" [("key", [Value "bar", InterpolationValue (lift "")])]

      it "parses unquoted attribute interpolation and does not mix it with values for short closed root element" $
         parseHtmlFromString "<html key=${foo} bar />" `shouldBe`
            ShortTag "html" [("key", [InterpolationValue (lift "")]), ("bar", [])]

      it "parses single quoted interpolation attribute for short closed root element" $
         parseHtmlFromString "<html key='hello ${name}' />" `shouldBe`
            ShortTag "html" [("key", [Value "hello ", InterpolationValue (lift "")])]

      it "parses double quoted interpolation attribute for spaced short closed root element" $
         parseHtmlFromString "<html key=\"hello ${name}\" />" `shouldBe`
            ShortTag "html" [("key", [Value "hello ", InterpolationValue (lift "")])]

   describe "CSS integration" $ do
      it "parses CSS in style double quoted attribute for root element" $
         parseHtmlFromString "<html style=\"text-align: center;\" />" `shouldBe`
            ShortTag "html" [("style", [StyleValue ("text-align", [Css.Value "center"])])]

      it "parses CSS in style single quoted attribute for root element" $
         parseHtmlFromString "<html style='text-align: center;' />" `shouldBe`
            ShortTag "html" [("style", [StyleValue ("text-align", [Css.Value "center"])])]

      it "parses CSS style tag" $
         parseHtmlFromString "<style>div { text-align: center; }</style>" `shouldBe`
            FullTag "style" [] [Content $ Style [Css.Block ["div"] [("text-align", [Css.Value "center"])]]]

   describe "Content" $ do
      it "parses root element with text content" $
         parseHtmlFromString "<html>text content</html>" `shouldBe`
            FullTag "html" [] [Content $ Plain "text", Content BreakingSpace, Content $ Plain "content"]

      it "parses root element with spaced text content" $
         parseHtmlFromString "<html> text content </html>" `shouldBe`
            FullTag "html" [] [Content BreakingSpace, Content $ Plain "text", Content BreakingSpace, Content $ Plain "content", Content BreakingSpace]

      it "parses root element with multi-spaced text content to be equal with one spaced" $
         parseHtmlFromString "<html>      text  content    </html>" `shouldBe`
            FullTag "html" [] [Content BreakingSpace, Content $ Plain "text", Content BreakingSpace, Content $ Plain "content", Content BreakingSpace]

      it "parses root element with text and element content" $
         parseHtmlFromString "<html>text <b>content</b></html>" `shouldBe`
            FullTag "html" [] [Content $ Plain "text", Content BreakingSpace, FullTag "b" [] [Content $ Plain "content"]]

      it "parses root element with text and element content with spaces" $
         parseHtmlFromString "<html>  text  <b>  content </b>   </html>" `shouldBe`
            FullTag "html" [] [Content BreakingSpace, Content $ Plain "text", Content BreakingSpace, FullTag "b" [] [Content BreakingSpace, Content $ Plain "content", Content BreakingSpace], Content BreakingSpace]

      it "parses root element with text and dangling element content" $
         parseHtmlFromString "<html>text <br>more</html>" `shouldBe`
            FullTag "html" [] [Content $ Plain "text", Content BreakingSpace, DanglingTag "br" [], Content $ Plain "more"]

      it "parses interpolation inside root element" $
         parseHtmlFromString "<html>${interpolation}</html>" `shouldBe`
            FullTag "html" [] [Content $ Interpolation (lift "")]

      it "parses interpolation within text inside root element" $
         parseHtmlFromString "<html>Hello, ${interpolation}!</html>" `shouldBe`
            FullTag "html" [] [Content $ Plain "Hello,", Content BreakingSpace, Content $ Interpolation (lift ""), Content $ Plain "!"]

      it "parses interpolation within text with space at the end inside root element" $
         parseHtmlFromString "<html>Hello, ${interpolation} mate!</html>" `shouldBe`
            FullTag "html" [] [Content $ Plain "Hello,", Content BreakingSpace, Content $ Interpolation (lift ""), Content BreakingSpace, Content $ Plain "mate!"]

   describe "Quasiquotes" $ do
      it "applies string interpolation inside root element" $
         let foo = "bar" in ([html|<html>${foo}</html>|] :: Html) `shouldBe`
            FullTag "html" [] [Content $ Plain "bar"]

      it "applies string interpolation inside root element and quotes html" $
         let foo = "eskimo < ben & jerry's" in ([html|<html>${foo}</html>|] :: Html) `shouldBe`
            FullTag "html" [] [Content $ Plain "eskimo &lt; ben &amp; jerry's"]

      it "applies string interpolation within text inside root element" $
         let name = "Maija" in ([html|<html>Hello ${name}!</html>|] :: Html) `shouldBe`
            FullTag "html" [] [Content $ Plain "Hello", Content BreakingSpace, Content $ Plain "Maija", Content $ Plain "!"]

      it "applies string interpolation within text inside root element" $
         let name = "Maija" in ([html|<html>Hello ${name}!</html>|] :: Html) `shouldBe`
            FullTag "html" [] [Content $ Plain "Hello", Content BreakingSpace, Content $ Plain "Maija", Content $ Plain "!"]

      it "applies integer interpolation inside root element" $
         let foo = 42 :: Int in ([html|<html>${foo}</html>|] :: Html) `shouldBe`
            FullTag "html" [] [Content $ Plain "42"]

      it "applies string interpolation for root element attribute" $
         let bar = "bar" in ([html|<html attr=foo${bar} />|] :: Html) `shouldBe`
            ShortTag "html" [("attr", [Value "foo", Value "bar"])]

      it "applies string interpolation for root element attribute and quotes single quotes" $
         let bar = "bar's" in ([html|<html attr='${bar}' />|] :: Html) `shouldBe`
            ShortTag "html" [("attr", [Value "bar&apos;s"])]

      it "applies string interpolation for root element attribute and quotes double quotes" $
         let bar = "he said \"get over it\"" in ([html|<html attr="${bar}" />|] :: Html) `shouldBe`
            ShortTag "html" [("attr", [Value "he said &quot;get over it&quot;"])]

      it "applies style interpolation for root element unquoted style attribute" $
         let bar = StyleValue ("font-size", [Css.Value "12px"]) in ([html|<html style=${bar} />|] :: Html) `shouldBe`
            ShortTag "html" [("style", [StyleValue ("font-size", [Css.Value "12px"])])]

      it "applies multiline interpolation" $
         let content = "foobar" in ([html|
            <body>
               ${content}
            </body>
         |] :: Html) `shouldBe`
            FullTag "body" [] [Content BreakingSpace, Content $ Plain "foobar", Content BreakingSpace]
