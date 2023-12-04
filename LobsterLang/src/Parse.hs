{-
-- EPITECH PROJECT, 2023
-- Parse.hs
-- File description:
-- Parse
-}

module Parse (
    Parser (..),
    parseChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parseTuple,
    parseAnyChar,
    initParseChar,
    initParseOr,
    initParseAnd,
    initParseAndWith,
    initParseMany,
    initParseSome,
    initParseUInt,
    initParseInt,
    initParseTuple,
    initParseAnyChar,
) where

import Data.Maybe(isNothing)
import Control.Applicative (Alternative (..))

type ParserType a = String -> Maybe (a, String)

newtype Parser a = Parser {
    runParser :: ParserType a
}

instance Functor Parser where
    fmap fct parser =
        Parser
        (
            \s -> case runParser parser s of
                Nothing -> Nothing
                Just (a, b) -> Just (fct a, b)
        )

instance Applicative Parser where
    pure result = Parser (\_ -> Just (result, ""))

    (<*>) parserA parserB =
        Parser
        (
            \s -> case runParser parserA s of
                Nothing -> Nothing
                Just (a, b) -> case runParser parserB b of
                    Nothing -> Nothing
                    Just (a', b') -> Just (a a', b')
        )

instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) parserA parserB =
        Parser
        (
            \s -> case runParser parserA s of
                Nothing -> runParser parserB s
                result -> result
        )


parseChar :: Char -> ParserType Char
parseChar _ [] = Nothing
parseChar c (x:xs)
    | x == c = Just (c, xs)
    | otherwise = Nothing

initParseChar :: Char -> Parser Char
initParseChar c = Parser (parseChar c)

parseOr :: Parser a -> Parser a -> ParserType a
parseOr parserA parserB = runParser (parserA <|> parserB)

initParseOr :: Parser a -> Parser a -> Parser a
initParseOr parserA parserB = Parser (parseOr parserA parserB)

parseAnd :: Parser a -> Parser b -> ParserType (a, b)
parseAnd parserA parserB s = case runParser parserA s of
    Nothing -> Nothing
    Just resultA -> runParser ((\b -> (fst resultA, b)) <$> parserB) (snd resultA)

initParseAnd :: Parser a -> Parser b -> Parser (a, b)
initParseAnd parserA parserB = Parser (parseAnd parserA parserB)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> ParserType c
parseAndWith f parserA parserB s = case parseAnd parserA parserB s of
    Nothing -> Nothing
    Just ((a, b), s') -> Just (f a b, s')

initParseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
initParseAndWith f parserA parserB = Parser (parseAndWith f parserA parserB)

parseMany :: Parser a -> ParserType [a]
parseMany parser s = case runParser parser s of
    Nothing -> Just ([] , s)
    Just (a, b) -> case parseMany parser b of
        Nothing -> Just ([a], b)
        Just (a', b') -> Just (a : a', b')

initParseMany :: Parser a -> Parser [a]
initParseMany parserA = Parser (parseMany parserA)

parseSome :: Parser a -> ParserType [a]
parseSome parser = runParser ((:) <$> parser <*> initParseMany parser)

initParseSome :: Parser a -> Parser [a]
initParseSome parserA = Parser (parseSome parserA)

parseUInt :: ParserType Int
parseUInt s = case parseSome (initParseAnyChar ['0'..'9']) s of
    Nothing -> Nothing
    Just ([], _) -> Nothing
    Just (a, b) -> Just (read a :: Int, b)

initParseUInt :: Parser Int
initParseUInt = Parser parseUInt

parseInt :: ParserType Int
parseInt ('-':xs) = runParser ((\x -> -x) <$> initParseUInt) xs
parseInt ('+':xs) = parseUInt xs
parseInt s = parseUInt s

initParseInt :: Parser Int
initParseInt = Parser parseInt

parseTuple :: Parser a -> ParserType (a, a)
parseTuple parser ('(':xs) = case parseAnd parser (initParseChar ',') xs of
    Nothing -> Nothing
    Just ((nb, _), b) -> case parseAnd parser (initParseChar ')') b of
        Nothing -> Nothing
        Just ((nb', _), c) -> Just ((nb, nb'), c)
parseTuple _ _ = Nothing

-- parseList :: Parser a -> Parser [a]
-- parseList parser ('(':xs) = case 

initParseTuple :: Parser a -> Parser (a, a)
initParseTuple parser = Parser (parseTuple parser)

parseAnyChar :: String -> ParserType Char
parseAnyChar [] _ = Nothing
parseAnyChar (x:xs) s
    | isNothing parsed = parseAnyChar xs s
    | otherwise = parsed
        where parsed = parseOr (initParseChar x) (initParseChar c) s
              c
                | null xs = '\0'
                | otherwise = head xs

initParseAnyChar :: String -> Parser Char
initParseAnyChar s = Parser (parseAnyChar s)
