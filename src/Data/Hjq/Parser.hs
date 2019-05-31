{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Parser where

import Data.Attoparsec.Text
import Data.Text
import Control.Applicative

data JqFilter
    = JqField Text JqFilter
    | JqIndex Int JqFilter
    | JqNil
    deriving (Show, Read, Eq)

data JqQuery
    = JqQueryObject [(Text, JqQuery)]
    | JqQueryArray [JqQuery]
    | JqQueryFilter JqFilter
    deriving (Show, Read, Eq)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace

-- Parse user input
parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter s = showParseResult
    $ parse (jqFilterParser <* endOfInput) s `feed` ""

-- Parse filtered literals to `JqFilter` type with attoparsec.
jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
    where
        jqFilter :: Parser JqFilter
        jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

        jqField :: Parser JqFilter
        jqField = JqField <$> (word <* skipSpace) <*> jqFilter

        jqIndex = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

-- Show parse result.
showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack $ show r

-- Parser which parses identifiers(fieldName etc.).
word :: Parser Text
word = fmap pack $ many1 (letter <|> char '-' <|> char '_' <|> digit)

-- Run parser
parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery s = showParseResult $ parse (jqQueryParser <* endOfInput) s `feed` ""

-- Main part of parser
-- Parser which parses query literals with attoparsec.
jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryFilter <|> queryObject
    where
        queryArray :: Parser JqQuery
        queryArray = JqQueryArray <$>  (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')

        queryObject :: Parser JqQuery
        queryObject = JqQueryObject <$> (schar '{' *> (qObj `sepBy` schar ',') <* schar '}')

        qObj :: Parser (Text, JqQuery)
        qObj = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)

        queryFilter :: Parser JqQuery
        queryFilter = JqQueryFilter <$> jqFilterParser
