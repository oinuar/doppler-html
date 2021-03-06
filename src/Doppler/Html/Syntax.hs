module Doppler.Html.Syntax (
   parseHtml, parseHtmlFromString, html, style
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
import Data.Maybe                 (maybeToList)

-- HTML tree structure parser.
parseHtml :: Parser Html
             -- ^ HTML parser.
parseHtml = do
   tags <- parseTag parseTagName
                    parseAttributeName
                    parseAttributeValue
                    parseContent
                    parseWhitespace

   return $ case tags of
      [x] -> Html x
      xs -> HtmlSiblings $ map Html xs

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

parseAttributeValue :: Quote -> HtmlAttributeName -> Parser HtmlAttributeValues
parseAttributeValue Unquoted _ =
   -- Attribute value must not contain any literal ASCII whitespace, any
   -- U+0022 QUOTATION MARK characters ("), U+0027 APOSTROPHE characters
   -- ('), U+003D EQUALS SIGN characters (=), U+003C LESS-THAN SIGN
   -- characters (<), U+003E GREATER-THAN SIGN characters (>), or U+0060
   -- GRAVE ACCENT characters (`), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationAttribute <$> parseInterpolationExpr

      value =
         AttributeValues . pure . Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf " \"'=<>`") unexpected x)

parseAttributeValue SingleQuotes "style" =
   -- Use CSS property parser when parsing style attribute value.
   AttributeValues . pure . StyleValue <$> parseCssProperty

parseAttributeValue DoubleQuotes "style" =
   -- Use CSS property parser when parsing style attribute value.
   AttributeValues . pure . StyleValue <$> parseCssProperty

parseAttributeValue SingleQuotes _ =
   -- Attribute value must not contain any literal U+0027 APOSTROPHE
   -- characters ('), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationAttribute <$> parseInterpolationExpr

      value =
         AttributeValues . pure . Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf "'") unexpected x)

parseAttributeValue DoubleQuotes _ =
   -- Attribute value must not contain any literal U+0022 QUOTATION
   -- MARK characters ("), and must not be the empty string.
   interpolation <|> value
   where
      interpolation =
         InterpolationAttribute <$> parseInterpolationExpr

      value =
         AttributeValues . pure . Value <$> many1 (do
            x <- optionMaybe (lookAhead $ string "${")
            maybe (noneOf "\"") unexpected x)

parseContent :: TagName -> Parser HtmlContent
parseContent "style" =
   -- Use CSS parser when parsing style tag contents.
   Style <$> many1 parseCss

parseContent _ =
   -- The text in raw text and escapable raw text elements must not
   -- contain any occurrences of the string "</" (U+003C LESS-THAN SIGN,
   -- U+002F SOLIDUS) followed by characters that case-insensitively match
   -- the tag name of the element followed by one of U+0009 CHARACTER
   -- TABULATION (tab), U+000A LINE FEED (LF), U+000C FORM FEED (FF), U+000D
   -- CARRIAGE RETURN (CR), U+0020 SPACE, U+003E GREATER-THAN SIGN (>), or
   -- U+002F SOLIDUS (/).
   interpolation <|> breakingSpace <|> plain
   where
      interpolation =
         Interpolation <$> parseInterpolationExpr

      breakingSpace = do
         _ <- parseWhitespace
         skipMany parseWhitespace
         return BreakingSpace

      plain =
         Plain <$> many1 plainChar

      plainChar = do
         x <- optionMaybe (lookAhead $ (pure <$> parseWhitespace) <|> tag <|> string "${")
         maybe anyChar unexpected x

      tag = do
         a <- char '<'
         b <- optionMaybe $ char '/'
         c <- parseTagName
         d <- parseWhitespace <|> maybe (oneOf "/>") (const $ char '>') b
         return $ [a] ++ maybeToList b ++ c ++ [d]

-- | Quasiquoter for HTML syntax.
html :: QuasiQuoter
html = QuasiQuoter {
   quoteExp = compileHtmlExpression,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}

-- | Quasiquoter for CSS style properties.
style :: QuasiQuoter
style = QuasiQuoter {
   quoteExp = compileStyleExpression,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}

compileHtmlExpression :: String -> Q Exp
compileHtmlExpression str =
   case parse parseHtml str str of
      Right x -> lift x
      Left err -> fail $ show err

compileStyleExpression :: String -> Q Exp
compileStyleExpression str =
   case parse (many parseCssProperty) str str of
      Right x -> lift x
      Left err -> fail $ show err

mkQExp :: String -> Q Exp
mkQExp =
   foldl1 appE . map (varE . mkName) . words

parseInterpolationExpr :: Parser (Q Exp)
parseInterpolationExpr =
   mkQExp <$> between (string "${") (char '}') (many1 $ noneOf "}")

parseWhitespace :: Parser Char
parseWhitespace =
   space <|> tab <|> endOfLine
