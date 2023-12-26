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
    parseTest,
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

-- | Instance Functor of the data Parser
instance Functor Parser where
    fmap fct parser =
        Parser
        (
            \s -> case runParser parser s of
                Nothing -> Nothing
                Just (a, b) -> Just (fct a, b)
        )

-- | Instance Applicative of the data Parser
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

-- | Instance Alternative of the data Parser
instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) parserA parserB =
        Parser
        (
            \s -> case runParser parserA s of
                Nothing -> runParser parserB s
                result -> result
        )


-- | Instance Monad of the data Parser
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= b =
        Parser
        (
            \s -> case runParser a s of
                Nothing -> Nothing
                Just (res, s') -> runParser (b res) s'
        )

-- | Parse a character c
-- Takes the character that need to be parsed
-- Returns a data Parser that contain the character and the rest of the string
parseChar :: Char -> Parser Char
parseChar c = Parser (f c)
    where
        f :: Char -> String -> Maybe (Char, String)
        f char (x:xs) = if char == x then Just (char, xs) else Nothing
        f _ _ = Nothing

-- | Parse with the first or the second parser
-- Takes two parsers
-- Returns either the first parser or the second parser
parseOr :: Parser a -> Parser a -> Parser a
parseOr parserA parserB = parserA <|> parserB

-- | Parse with the first and the second parser
-- Takes two parsers
-- Returns either the first parser then use result for the second parser
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parserA parserB = Parser (f parserA parserB)
    where
        f :: Parser a -> Parser b -> String -> Maybe ((a, b), String)
        f pA pB s = case runParser pA s of
            Nothing -> Nothing
            Just resultA ->
                runParser ((\b -> (fst resultA, b)) <$> pB) (snd resultA)

-- | Parse with function after the two parsers
-- Takes two parsers and a function
-- Returns the result of the function with the result of the parseAnd
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f' parserA parseB = Parser (f f' parserA parseB)
    where
    f :: (a -> b -> c) -> Parser a -> Parser b ->String -> Maybe (c, String)
    f f'' pA pB s = case runParser (parseAnd pA pB) s of
        Nothing -> Nothing
        Just ((a, b), s') -> Just (f'' a b, s')

-- | Parse with a parser
-- Takes a parser
-- Returns the application of the parser (if nothing, returns an empty list)
parseMany :: Parser a -> Parser [a]
parseMany parserA = Parser (f parserA)
    where
        f :: Parser a -> String -> Maybe ([a], String)
        f parser s = case runParser parser s of
            Nothing -> Just ([] , s)
            Just (a, b) -> case runParser (parseMany parser) b of
                Nothing -> Just ([a], b)
                Just (a', b') -> Just (a : a', b')

-- | Parse with a parser
-- Takes a parser
-- Returns the application of the parser at least one time or Returns Nothing
parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

-- | Return a data Parser that parse a UInt
parseUInt :: Parser Int
parseUInt = Parser f
    where
        f :: String -> Maybe (Int, String)
        f s = case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
            Nothing -> Nothing
            Just ([], _) -> Nothing
            Just (a, b) -> Just (read a :: Int, b)

-- | Return a data Parser that parse a '-' or '+'
parseSign :: Parser Char
parseSign = parseChar '-' <|> parseChar '+'

-- | Return a data Parser that parse a digit
parseDigit :: Parser Char
parseDigit = parseChar '0' <|> parseChar '1' <|> parseChar '2' <|>
             parseChar '3' <|> parseChar '4' <|> parseChar '5' <|>
             parseChar '6' <|> parseChar '7' <|> parseChar '8' <|>
             parseChar '9'

-- | Return a data Parser that parse a Int
parseInt :: Parser Int
parseInt = Parser f
    where
        f :: String -> Maybe (Int, String)
        f ('-':xs) = runParser ((\x -> -x) <$> parseUInt) xs
        f s = runParser parseUInt s

-- | Return a data Parser that parse multiple space
parseSpace :: Parser [Char]
parseSpace = parseMany (parseChar ' ')

-- | Parse with a parser and, if possible with a space
-- Return a Parser that parse element with the given parser and, if possible with multiple space
parseElem :: Parser a -> Parser a
parseElem parser = parseAndWith (\x _ -> x) parser parseSpace <|> parser

-- | Return a data Parser that parse a String
parseString :: Parser String
parseString = parseSpace *> parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ "-*/%+#")) <* parseSpace

-- | Return a data Parser that parse a String as a Symbol
parseSymbol :: Parser SExpr
parseSymbol = Symbol <$> parseElem parseString

-- | Return a data Parser that parse a Int as a Value
parseValue :: Parser SExpr
parseValue = Value <$> parseElem parseInt

-- | Parse a list of element
-- Return a Parser of list `element` that start with a '(' and end with a ')'
parseList :: Parser a -> Parser [a]
parseList parser = parseStart *> parseListValue <* parseEnd
    where
        parseEnd = parseChar ')' <* parseSpace
        parseListValue = parseSpace *> parseMany (parseElem parser)
        parseStart = parseSpace *> parseChar '('

-- | Parse any characterfrom a String
-- Return a Parser that parse every character from a String
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

-- | Return a Parser that parse a Bool (#f or #t)
parseBool :: Parser Bool
parseBool = parseElem (Parser f)
    where
        f :: String -> Maybe (Bool, String)
        f ('#':'f':' ':xs) = Just (False, xs)
        f ('#':'t':' ':xs) = Just (True, xs)
        f _ = Nothing

-- | Return a Parser that parse a SExpr
parseSExpr :: Parser SExpr
parseSExpr = List <$> parseList (parseSpace *> parseValue <|> parseSymbol <|> parseSpace *> parseSExpr)
            <|> parseSymbol
            <|> parseValue

-- | Return a Result that contain the evaluation of our Lisp String
-- Takes as parameter the string that need to be evaluated and the Stack (Environment)
parseLisp :: String -> [Scope.ScopeMb] -> (Maybe AST.Ast, [Scope.ScopeMb])
parseLisp s stack = case runParser parseSExpr s of
    Nothing -> (Nothing, [])
    Just (res, _) -> case AstEval.sexprToAst res of
        Nothing -> (Nothing, [])
        Just value -> AstEval.evalAst stack value

parseTest :: String -> [Scope.ScopeMb] -> (Maybe AST.Ast, [Scope.ScopeMb])
parseTest s stack = case runParser parseSExpr s of
    Nothing -> (Nothing, [])
    Just (res, _) -> case AstEval.sexprToAst res of
        Nothing -> (Nothing, [])
        Just value -> (Just value, stack)
