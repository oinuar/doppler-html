module Doppler.Html.Syntax (
   parseHtml, parseHtmlFromString, html
) where

import Doppler.Html.Types
import Doppler.Tag.Syntax
import Doppler.Css.Syntax
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Doppler.Tag.Types          (Quote (..))
import Text.Parsec.String         (Parser)
import Language.Haskell.TH.Quote  (QuasiQuoter (..))
import Control.Monad              (when)

-- HTML tree structure parser.
parseHtml :: Parser Html
             -- ^ HTML parser.
parseHtml = do
   tags <- parseTag parseTagName
                    parseAttributeName
                    parseAttributeValue
                    parseContent

   if null tags || length tags > 1 then
      unexpected "source must contain exactly one root \
                  \tag that encloses all other tags"
   else
      return $ head tags

-- Parses HTML from string.
parseHtmlFromString :: String ->
                       -- ^ String to parse.
                       Html
                       -- ^ Parsed Html.
parseHtmlFromString source =
   case parse parseHtml source source of
      Right x -> x
      Left x -> error . show $ x

parseTagName :: Parser HtmlTagName
parseTagName =
   -- Html element all have names that only use characters
   -- in the range 0-9, a-z and A-Z.
   many1 $ digit <|> lower <|> upper

parseAttributeName :: Parser HtmlAttributeName
parseAttributeName =
   -- Attribute names must consist of one or more characters
   -- other than the ASCII whitespace, U+0000 NULL, U+0022 QUOTATION
   -- MARK ("), U+0027 APOSTROPHE ('), U+003E GREATER-THAN SIGN (>),
   -- U+002F SOLIDUS (/), and U+003D EQUALS SIGN (=) characters, the
   -- control characters, and any characters that are not defined by
   -- Unicode.
   many1 $ noneOf " \0\"'>/=\n\r\t"

parseAttributeValue :: Quote -> HtmlAttributeName -> Parser HtmlAttributeValue
parseAttributeValue Unquoted _ =
   -- Attribute value must not contain any literal ASCII whitespace, any
   -- U+0022 QUOTATION MARK characters ("), U+0027 APOSTROPHE characters
   -- ('), U+003D EQUALS SIGN characters (=), U+003C LESS-THAN SIGN
   -- characters (<), U+003E GREATER-THAN SIGN characters (>), or U+0060
   -- GRAVE ACCENT characters (`), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationValue <$> parseInterpolationExpr

      value =
         Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf " \"'=<>`") unexpected x)

parseAttributeValue SingleQuotes "style" =
   -- Use CSS property parser when parsing style attribute value.
   StyleValue <$> parseCssProperty

parseAttributeValue DoubleQuotes "style" =
   -- Use CSS property parser when parsing style attribute value.
   StyleValue <$> parseCssProperty

parseAttributeValue SingleQuotes _ =
   -- Attribute value must not contain any literal U+0027 APOSTROPHE
   -- characters ('), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationValue <$> parseInterpolationExpr

      value =
         Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf "'") unexpected x)

parseAttributeValue DoubleQuotes _ =
   -- Attribute value must not contain any literal U+0022 QUOTATION
   -- MARK characters ("), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationValue <$> parseInterpolationExpr

      value =
         Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf "\"") unexpected x)

parseContent :: TagName -> Parser HtmlContent
parseContent "style" =
   -- Use CSS parser when parsing style tag contents.
   Style <$> parseCss

parseContent _ =
   -- The text in raw text and escapable raw text elements must not
   -- contain any occurrences of the string "</" (U+003C LESS-THAN SIGN,
   -- U+002F SOLIDUS) followed by characters that case-insensitively match
   -- the tag name of the element followed by one of U+0009 CHARACTER
   -- TABULATION (tab), U+000A LINE FEED (LF), U+000C FORM FEED (FF), U+000D
   -- CARRIAGE RETURN (CR), U+0020 SPACE, U+003E GREATER-THAN SIGN (>), or
   -- U+002F SOLIDUS (/).
   interpolation <|> text
   where
      interpolation =
         Interpolation <$> parseInterpolationExpr

      text =
         Text <$> manyTill anyChar (lookAhead $ tag <|> string "${")

      tag = do
         _ <- char '<'
         _ <- optional $ char '/'
         _ <- parseTagName
         _ <- oneOf "\t\n>/"
         return mempty

-- | Quasiquoter for HTML syntax.
html :: QuasiQuoter
html = QuasiQuoter {
   quoteExp = compileExpression,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}

compileExpression :: String -> Q Exp
compileExpression str =
   case parse parseHtml str str of
      Right x -> lift x
      Left err -> fail $ show err

mkQExp :: String -> Q Exp
mkQExp =
   foldl1 appE . map (varE . mkName) . words

parseInterpolationExpr :: Parser (Q Exp)
parseInterpolationExpr =
   mkQExp <$> between (string "${") (char '}') (many1 $ noneOf "}")
