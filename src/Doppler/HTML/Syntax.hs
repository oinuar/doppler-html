module Doppler.HTML.Syntax (
   parseExpression, parseFromString, html
) where

import Doppler.CSS.Syntax
import Doppler.HTML.Types
import Text.Parsec
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Parsec.String        (Parser)
import Data.Char                 (toUpper)
import Language.Haskell.TH.Quote (QuasiQuoter (..))


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
   case parse parseExpression str str of
      Right x -> lift x
      Left err -> fail $ show err

parseFromString :: String -> Expression
parseFromString source =
   case parse parseElement source source of
      Right x -> x
      Left x -> error . show $ x

parseExpression :: Parser Expression
parseExpression =
   parseJunk *> parseElement <* eof

parseElement :: Parser Expression
parseElement = do
   (name, attrs, isShort) <- skipLineEnds openTag

   if isShort
      then return $ Element name attrs []
      else do
         childs <- children name
         close <- skipLineEnds closeTag
         return $ Element close attrs childs
   where
      children parentTagName = do
         name <- optionMaybe $ try (lookAhead closeTag)

         case name of
            Just name' -> if name' == parentTagName
                             then return []
                             else unexpected $ "closing tag " ++ name' ++ " does not match opening tag " ++ parentTagName
            Nothing -> do
               child <- parseElement <|> parseInclude <|> parseText
               isEof <- option False $ fmap (const True) eof

               if isEof
                  then unexpected $ "unexpected end, " ++ parentTagName ++ " is not closed"
                  else do
                     rest <- children parentTagName
                     return $ child:rest

      openTag = do
         _ <- lexeme $ char '<'
         name <- tagName
         attrs <- parseAttribute `manyTill` lookAhead closeAndTellIfShort
         closing <- lexeme closeAndTellIfShort
         return (name, attrs, closing)

      closeTag = do
         _ <- lexeme $ string "</"
         name <- tagName
         _ <- lexeme $ char '>'
         return name

      closeAndTellIfShort = do
         close <- string "/>" <|> fmap pure (char '>')
         return $ close == "/>"

      tagName = lexeme $ do
         firstLetter <- lower <|> upper
         restLetters <- many $ lower <|> upper <|> digit
         return $ map toUpper (firstLetter:restLetters)

parseInclude :: Parser Expression
parseInclude =
   Include <$> between (string "${") (char '}') characters
   where
      characters =
         many1 $ alphaNum <|> char ' '

parseText :: Parser Expression
parseText =
   lexeme $ fmap Text (many $ noneOf "<>$")

parseAttribute :: Parser Attribute
parseAttribute = do
   k <- key
   _ <- lexeme $ char '='
   v <- parseAttributeValues k
   return $ Attribute (k, v)
   where
      key = lexeme $ do
         first <- letter
         rest <- many $ alphaNum <|> char '-'
         return $ first:rest

parseAttributeValues :: Key -> Parser Collection
parseAttributeValues key =
   lexeme $ parseAttributeValues' <|> parseInterpolation
   where
      parseAttributeValues' =
         Values <$> case key of
            "style" -> between quotationMark quotationMark css
            _ -> between quotationMark quotationMark text

      parseInterpolation = do
         _ <- char '$'
         content <- parseInterpolationContent
         return $ Interpolation content

      css =
         cssProperty <|> pure []

      text =
         interpolation <|> characters <|> pure []

      cssProperty = do
         props <- parseProperties
         rest <- css
         return $ map CSSValue props ++ rest

      interpolation = do
         _ <- char '$'
         content <- parseInterpolationContent
         rest <- text
         return $ InterpolationValue content : rest

      characters = do
         content <- many1 $ noneOf "\"$"
         rest <- text
         return $ StringValue content : rest

      quotationMark =
         char '"'

parseInterpolationContent :: Parser String
parseInterpolationContent =
   between (char '{') (char '}') characters
   where
      characters =
         many1 $ alphaNum <|> char ' '

parseJunk :: Parser String
parseJunk =
   many $ space <|> tab <|> endOfLine

parseLineEnds :: Parser String
parseLineEnds =
   many endOfLine

lexeme :: Parser a -> Parser a
lexeme p =
   p <* many (space <|> tab)

skipLineEnds :: Parser a -> Parser a
skipLineEnds p =
   p <* parseLineEnds
