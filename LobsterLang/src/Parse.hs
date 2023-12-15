{-
-- EPITECH PROJECT, 2023
-- Parse.hs
-- File description:
-- Parse
-}
{-# LANGUAGE InstanceSigs #-}

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
    parseAnyChar,
    parseList,
    parseString,
    parseSign,
    parseDigit,
    parseBool,
    parseSExpr,
    parseSymbol,
    parseElem,
    parseValue,
    parseLisp,
    -- parseTuple,
) where

import SExpr

import Control.Applicative (Alternative (..))
import qualified AstEval
import qualified AST
import qualified Scope

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)

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


instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= b =
        Parser
        (
            \s -> case runParser a s of
                Nothing -> Nothing
                Just (res, s') -> runParser (b res) s'
        )

parseChar :: Char -> Parser Char
parseChar c = Parser (f c)
    where
        f :: Char -> String -> Maybe (Char, String)
        f char (x:xs) = if char == x then Just (char, xs) else Nothing
        f _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr parserA parserB = parserA <|> parserB

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parserA parserB = Parser (f parserA parserB)
    where
        f :: Parser a -> Parser b -> String -> Maybe ((a, b), String)
        f pA pB s = case runParser pA s of
            Nothing -> Nothing
            Just resultA ->
                runParser ((\b -> (fst resultA, b)) <$> pB) (snd resultA)

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f' parserA parseB = Parser (f f' parserA parseB)
    where
    f :: (a -> b -> c) -> Parser a -> Parser b ->String -> Maybe (c, String)
    f f'' pA pB s = case runParser (parseAnd pA pB) s of
        Nothing -> Nothing
        Just ((a, b), s') -> Just (f'' a b, s')

parseMany :: Parser a -> Parser [a]
parseMany parserA = Parser (f parserA)
    where
        f :: Parser a -> String -> Maybe ([a], String)
        f parser s = case runParser parser s of
            Nothing -> Just ([] , s)
            Just (a, b) -> case runParser (parseMany parser) b of
                Nothing -> Just ([a], b)
                Just (a', b') -> Just (a : a', b')

parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

parseUInt :: Parser Int
parseUInt = Parser f
    where
        f :: String -> Maybe (Int, String)
        f s = case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
            Nothing -> Nothing
            Just ([], _) -> Nothing
            Just (a, b) -> Just (read a :: Int, b)

parseSign :: Parser Char
parseSign = parseChar '-' <|> parseChar '+'

parseDigit :: Parser Char
parseDigit = parseChar '0' <|> parseChar '1' <|> parseChar '2' <|>
             parseChar '3' <|> parseChar '4' <|> parseChar '5' <|>
             parseChar '6' <|> parseChar '7' <|> parseChar '8' <|>
             parseChar '9'

parseInt :: Parser Int
parseInt = Parser f
    where
        f :: String -> Maybe (Int, String)
        f ('-':xs) = runParser ((\x -> -x) <$> parseUInt) xs
        f s = runParser parseUInt s

parseSpace :: Parser [Char]
parseSpace = parseMany (parseChar ' ')

parseElem :: Parser a -> Parser a
parseElem parser = parseAndWith (\x _ -> x) parser parseSpace <|> parser

parseString :: Parser String
parseString = parseSpace *> parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "-*/%+#")) <* parseSpace

parseSymbol :: Parser SExpr
parseSymbol = Symbol <$> parseElem parseString

parseValue :: Parser SExpr
parseValue = Value <$> parseElem parseInt

parseList :: Parser a -> Parser [a]
parseList parser = parseStart *> parseListValue <* parseEnd
    where
        parseEnd = parseChar ')' <* parseSpace
        parseListValue = parseSpace *> parseMany (parseElem parser)
        parseStart = parseSpace *> parseChar '('

parseAnyChar :: String -> Parser Char
parseAnyChar s = Parser (f s)
    where
        f :: String -> String -> Maybe (Char, String)
        f [] _ = Nothing
        f (x:xs) s' = case parsed of
            Nothing -> runParser (parseAnyChar xs) s'
            _ -> parsed
            where
                parsed = runParser (parseOr (parseChar x) (parseChar c)) s'
                c = case xs of
                    [] -> '\0'
                    _ -> head xs

parseBool :: Parser Bool
parseBool = parseElem (Parser f)
    where
        f :: String -> Maybe (Bool, String)
        f ('#':'f':' ':xs) = Just (False, xs)
        f ('#':'t':' ':xs) = Just (True, xs)
        f _ = Nothing

parseSExpr :: Parser SExpr
parseSExpr = List <$> parseList (parseSpace *> parseValue <|> parseSymbol <|> parseSpace *> parseSExpr)

parseLisp :: String -> (Maybe AST.Ast, [Scope.ScopeMb])
parseLisp s = case runParser parseSExpr s of
    Nothing -> (Nothing, [])
    Just (res, _) -> case AstEval.sexprToAst res of
        Nothing -> (Nothing, [])
        Just value -> AstEval.evalAst [] value
