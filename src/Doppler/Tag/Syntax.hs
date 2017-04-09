module Doppler.Tag.Syntax (
   parseTag
) where

import Doppler.Tag.Types
import Text.Parsec
import Text.Parsec.String     (Parser)
import Control.Monad          (void)

-- Generic tag structure parser.
parseTag :: Monoid v =>
            Parser TagName ->
            -- ^ Parser for tag names.
            Parser k ->
            -- ^ Parse for attribute keys.
            (Quote -> k -> Parser v) ->
            -- ^ Parser for attribute values.
            (TagName -> Parser c) ->
            -- ^ Parser for tag contents.
            Parser Char ->
            -- ^ Parser for whitespace characters.
            Parser [Tag (k, v) c]
            -- ^ Tag structure parser.
parseTag tagName attrName attrValue content whitespace =
   many whitespace *> tag
   where
      tag = do
         _ <- char '<'
         name <- tagName <* many whitespace
         attributes <- parseAttribute whitespace attrName attrValue `sepEndBy` many1 whitespace
         closing <- string "/>" <|> string ">"

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

      tagOrContent name =
         tag <|> many (parseContent content name)

      endTagName =
         between (string "</") (char '>') (tagName <* many whitespace)

parseAttribute :: Monoid v => Parser Char -> Parser k -> (Quote -> k -> Parser v) -> Parser (k, v)
parseAttribute whitespace key value = do
   k <- key <* many whitespace
   equal <- optionMaybe $ char '=' <* many whitespace
   v <- maybe (return mempty)
              (const $  between doubleQuote doubleQuote (many $ value DoubleQuotes k)
                    <|> between singleQuote singleQuote (many $ value SingleQuotes k)
                    <|> many (value Unquoted k))
              equal
   return (k, mconcat v)
   where
      singleQuote = char '\''
      doubleQuote = char '"'

parseContent :: (TagName -> Parser b) -> TagName -> Parser (Tag a b)
parseContent content name =
   Content <$> content name
