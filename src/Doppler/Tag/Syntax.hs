module Doppler.Tag.Syntax (
   parseTag
) where

import Doppler.Tag.Types
import Text.Parsec
import Text.Parsec.String     (Parser)
import Control.Monad          (void)

-- Generic tag structure parser.
parseTag :: Parser TagName ->
            -- ^ Parser for tag names.
            Parser k ->
            -- ^ Parse for attribute keys.
            (Quote -> k -> Parser v) ->
            -- ^ Parser for attribute values.
            (TagName -> Parser c) ->
            -- ^ Parser for tag contents.
            Parser [Tag (k, [v]) c]
            -- ^ Tag structure parser.
parseTag tagName attrName attrValue content = do
   _ <- try $ many parseWhitespace *> char '<'
   name <- tagName <* many parseWhitespace
   attributes <- parseAttribute attrName attrValue `sepEndBy` many1 parseWhitespace
   closing <- (string "/>" <|> string ">") <* many parseWhitespace

   -- Make a short tag if it is explicitly closed.
   if closing == "/>" then
      return [ShortTag name attributes]

   -- Parse full or dangling tag structures.
   else do
      rest <- manyTill (tagOrContent name) $ try (void (lookAhead endTagName) <|> eof)
      endName <- optionMaybe $ lookAhead endTagName

      -- Make a full tag if it is closed properly. Consume end tag
      -- name because this tag is closed.
      if maybe False (== name) endName then
         endTagName *> return [FullTag name attributes $ concat rest]

      -- Make a dangling tag if it is implicitly short. Leave end tag
      -- because parent tag closes it.
      else
         return $ DanglingTag name attributes : concat rest
   where
      tagOrContent name =
             parseTag tagName attrName attrValue content
         <|> parseContent content name

      endTagName =
         between (string "</")
                 (char '>' <* many parseWhitespace)
                 (tagName <* many parseWhitespace)

parseAttribute :: Parser k -> (Quote -> k -> Parser v) -> Parser (k, [v])
parseAttribute key value = do
   k <- key <* many parseWhitespace
   equal <- optionMaybe $ char '=' <* many parseWhitespace
   v <- maybe (return mempty)
              (const $  between doubleQuote doubleQuote (many $ value DoubleQuotes k)
                    <|> between singleQuote singleQuote (many $ value SingleQuotes k)
                    <|> many (value Unquoted k))
              equal
   return (k, v)
   where
      singleQuote = char '\''
      doubleQuote = char '"'

parseContent :: (TagName -> Parser b) -> TagName -> Parser [Tag a b]
parseContent content name = do
   content' <- content name
   return [Content content']

parseWhitespace :: Parser Char
parseWhitespace =
   space <|> tab <|> newline
