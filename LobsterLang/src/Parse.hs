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
    parseAst,
    parseElem,
    parseValue,
    parseLobster,
    parseAnyString,
    parseCmpString,
    parseDefineValue,
    parseProduct,
    parseSum,
    parseExpr,
    parseTrue,
    parseFalse,
    parseAstString,
    parseWhiteSpace,
    errorParsing,
    parseDefineFn,
    parseLambda,
    parseCond
) where

import qualified AST
import Control.Applicative
import Data.Maybe

type Position = (Int, Int)

data Parser a = Parser {
    runParser :: Position -> String -> Either String (a, String, Position)
}

data Token =  Number Int
            | Sym String
            | Identifier String
    deriving(Show, Eq)


-- | Instance Functor of the data Parser
instance Functor Parser where
    fmap fct parser =
        Parser
        (
            \pos s -> case runParser parser pos s  of
                Left err -> Left err
                Right (a, b, c) -> Right (fct a, b, c)
        )

-- | Instance Applicative of the data Parser
instance Applicative Parser where
    pure result = Parser (\pos s -> Right (result, s, pos))

    (<*>) parserA parserB =
        Parser
        (
            \pos s -> case runParser parserA pos s of
                Left err -> Left err
                Right (a, b, c) -> case runParser parserB c b of
                    Left err' -> Left err'
                    Right (a', b', c') -> Right (a a', b', c')
        )

-- | Instance Alternative of the data Parser
instance Alternative Parser where
    empty = Parser (\_ _ -> Left "Error on parsing")
    (<|>) parserA parserB =
        Parser
        (
            \pos s -> case runParser parserA pos s of
                Left _ -> runParser parserB pos s
                result -> result
        )


-- | Instance Monad of the data Parser
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= b =
        Parser
        (
            \pos s -> case runParser a pos s of
                Left err -> Left err
                Right (res, s', pos') -> runParser (b res) pos' s'
        )

errorParsing :: (Int, Int) -> String
errorParsing (row, col) = "Error on parsing on '" ++ show row ++ "' '" ++ show col ++ "'"

startCharacter :: String
startCharacter = ['a'..'z'] ++ ['A'..'Z'] ++ "_"

lobsterCharacter :: String
lobsterCharacter = startCharacter ++ ['0'..'9']

-- | Parse a character c
-- Takes the character that need to be parsed
-- Returns a data Parser that contain the character and the rest of the string
parseChar :: Char -> Parser Char
parseChar c = Parser (f c)
    where
        f :: Char -> Position -> String -> Either String (Char, String, Position)
        f '\n' (row, col) (x:xs)
            | x == '\n' = Right ('\n', xs, (row + 1, 0))
            | otherwise = Left (errorParsing (row, col))
        f char (row, col) (x:xs)
            | x == char = Right (char, xs, (row, col + 1))
            | otherwise = Left (errorParsing (row, col))
        f _ (row, col) _ = Left (errorParsing (row, col))

-- | Parse with the first or the second parser
-- Takes two parsers
-- Returns either the first parser or the second parser
parseOr :: Parser a -> Parser a -> Parser a
parseOr parserA parserB = parserA <|> parserB

-- | Parse with the first and the second parser
-- Takes two parsers
-- Returns either the first parser then use result for the second parser
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parserA parserB = parserA >>= \resA -> parserB >>= \resB -> return (resA, resB)

-- | Parse with function after the two parsers
-- Takes two parsers and a fh (\x _ -> x) unction
-- Returns the result of the function with the result of the parseAnd
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f parserA parserB = parseAnd parserA parserB >>= \(a, b) -> return (f a b)

-- | Parse with a parser
-- Takes a parser
-- Returns the application of the parser (if nothing, returns an empty list)
parseMany :: Parser a -> Parser [a]
parseMany parserA = Parser (f parserA)
    where
        f :: Parser a -> Position -> String -> Either String ([a], String, Position)
        f parser pos s = case runParser parser pos s of
            Left _ -> Right ([], s, pos)
            Right (res, s', pos') -> case runParser (parseMany parser) pos' s' of
                Left _ -> Right ([res], s', pos')
                Right (res', s'', pos'') -> Right (res : res', s'', pos'')

-- | Parse with a parser
-- Takes a parser
-- Returns the application of the parser at least one time or Returns Nothing
parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

-- | Return a data Parser that parse a UInt
parseUInt :: Parser Int
parseUInt = parseSome parseDigit >>= \value -> return (read value :: Int)

-- | Return a data Parser that parse a '-' or '+'
parseSign :: Parser Char
parseSign = parseChar '-' <|> parseChar '+'

-- | Return a data Parser that parse a digit
parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

-- | Return a data Parser that parse a Int
parseInt :: Parser Int
parseInt = Parser f
    where
        f :: Position -> String -> Either String (Int, String, Position)
        f pos ('-':xs) = runParser ((\x -> -x) <$> parseUInt) pos xs
        f pos s = runParser parseUInt pos s

parseWhiteSpace :: Parser [Char]
parseWhiteSpace = parseMany (parseAnyChar "\n\t " <|> parseComment)

-- | Parse with a parser and, if possible with a space
-- Return a Parser that parse element with the given parser and, if possible with multiple space
parseElem :: Parser a -> Parser a
parseElem parser = parseAndWith const parser parseWhiteSpace <|> parser


-- | Return a data Parser that parse a String
parseString :: Parser String
parseString = do value <- parseWhiteSpace *> parseSome (parseAnyChar startCharacter)
                 res <- optional (parseMany (parseAnyChar lobsterCharacter) >>= \res' -> return (value ++ res') <* parseWhiteSpace)
                 return $ fromMaybe value res

-- | Return a data Parser that parse a String as a Symbol
parseAstString :: Parser AST.Ast
parseAstString = AST.String <$> (parseChar '"' *> parseElem parseString <* parseChar '"')

parseExpr :: Parser AST.Ast
parseExpr = parseCombinatorOperator

parseCombinatorOperator :: Parser AST.Ast
parseCombinatorOperator = do res <- parseBoolOperator
                             res' <- optional (parseWhiteSpace *> parseChar '$'
                                            >>= \res' -> parseCombinatorOperator
                                                >>= \res'' -> return $ AST.Call [res'] [res, res''])
                             return $ fromMaybe res res'

parseBoolOperator :: Parser AST.Ast
parseBoolOperator = do res <- parseCompOperator
                       res' <- optional (parseAnyString "&&" <|>
                                         parseAnyString "||" <|>
                                         parseAnyString "^^"
                                            >>= \res' -> parseBoolOperator
                                                >>= \res'' -> return $ AST.Call res' [res, res''])
                       return $ fromMaybe res res'

parseCompOperator :: Parser AST.Ast
parseCompOperator = do res <- parseSum
                       res' <- optional (parseWhiteSpace *> parseAnyString "==" <|>
                                         parseWhiteSpace *> parseAnyString ">=" <|>
                                         parseWhiteSpace *> parseAnyString "!=" <|>
                                         parseWhiteSpace *> parseAnyString "<=" <|>
                                         parseWhiteSpace *> parseAnyString ">" <|>
                                         parseWhiteSpace *> parseAnyString "<"
                                            >>= \res' -> parseCompOperator
                                                >>= \res'' -> return $ AST.Call res' [res, res''])
                       return $ fromMaybe res res'

parseSum :: Parser AST.Ast
parseSum = do res <- parseProduct
              res' <- optional (parseWhiteSpace *> parseAnyChar "+-"
                                    >>= \res' -> parseSum
                                        >>= \res'' -> return $ AST.Call [res'] [res, res''])
              return $ fromMaybe res res'

parseProduct :: Parser AST.Ast
parseProduct = do res <- parseListOperator
                  res' <- optional (parseWhiteSpace *> parseAnyChar "*/%"
                                    >>= \res' -> parseProduct
                                        >>= \res'' -> return $ AST.Call [res'] [res, res''])
                  return $ fromMaybe res res'

parseListOperator :: Parser AST.Ast
parseListOperator = do res <- parseValue
                       res' <- optional (parseWhiteSpace *> parseAnyString "--" <|>
                                         parseWhiteSpace *> parseAnyString "++" <|>
                                         parseWhiteSpace *> parseAnyString "!!"
                                            >>= \res' -> parseListOperator
                                                >>= \res'' -> return $ AST.Call res' [res, res''])
                       return $ fromMaybe res res'

-- | Return a data Parser that parse a Int as a Value
parseValue :: Parser AST.Ast
parseValue = parseWhiteSpace *> (
                                 parseWhiteSpace *> parseAnyString "(|" *> parseExpr <* parseAnyString "|)" <* parseWhiteSpace
                                 <|> AST.Value <$> parseElem parseInt
                                 <|> parseBool
                                 <|> parseSymbol
                                 <|> parseAstString
                                 <|> parseUnaryOperator
                                 <|> parseAstList
                                )

parseUnaryOperator :: Parser AST.Ast
parseUnaryOperator = parseWhiteSpace *> parseAnyString "!"<|>
                     parseWhiteSpace *> parseAnyString "@" <|>
                     parseWhiteSpace *> parseAnyString "~"
                        >>= \op -> parseValue
                                        >>= \value -> return $ AST.Call op [value]

parseListElem :: Parser a -> Parser [a]
parseListElem parserA = Parser (parseFirst parserA)
    where
        parseFirst :: Parser a -> Position -> String -> Either String ([a], String, Position)
        parseFirst parser pos s = case runParser parser pos s of
            Left _ -> Right ([], s, pos)
            Right (res, s', pos') -> case parseOthers parser pos' s' of
                Left err -> Left err
                Right (res', s'', pos'') -> Right (res : res', s'', pos'')
        parseOthers :: Parser a -> Position -> String -> Either String ([a], String, Position)
        parseOthers parser pos s = case runParser (parseChar ',') pos s of
            Left _ -> Right ([], s, pos)
            Right (_, s', pos') -> case runParser parser pos' s' of
                Left err -> Left err
                Right (res, s'', pos'') -> case parseOthers parser pos'' s'' of
                    Left err -> Left err
                    Right (res', s''', pos''') -> Right (res : res', s''', pos''')

-- | Parse a list of element
-- Return a Parser of list `element` that start with a '(' and end with a ')'
parseList :: Parser a -> String -> String -> Parser [a]
parseList parser start end = parseStart *> parseListValue <* parseEnd
    where
        parseEnd = parseAnyString end <* parseWhiteSpace
        parseListValue = parseWhiteSpace *> parseListElem parser <* parseWhiteSpace
        parseStart = parseWhiteSpace *> parseAnyString start

-- | Parse any character from a String
-- Return a Parser that parse every character from a String
parseAnyChar :: String -> Parser Char
parseAnyChar s = Parser (f s)
    where
        f :: String -> Position -> String -> Either String (Char, String, Position)
        f [] (row, col) _ = Left (errorParsing (row, col))
        f (x:xs) pos s' = case parsed of
            Left _ -> runParser (parseAnyChar xs) pos s'
            _ -> parsed
            where
                parsed = runParser (parseOr (parseChar x) (parseChar c)) pos s'
                c = case xs of
                    [] -> '\0'
                    _ -> head xs

parseAstList :: Parser AST.Ast
parseAstList = AST.List <$> parseList parseAst "[|" "|]"

-- | Parse a specific String
parseAnyString :: String -> Parser String
parseAnyString s = Parser (f s s)
    where
        f :: String -> String -> Position  -> String -> Either String (String, String, Position)
        f (x:xs) str pos s' = case runParser (parseChar x) pos s' of
            Left err -> Left err
            Right (_, s'', pos') -> f xs str pos' s''
        f [] str pos s' = Right (str, s', pos)

parseCmpString :: String -> Parser String
parseCmpString s = Parser (f s)
    where
        f :: String -> Position -> String -> Either String (String, String, Position)
        f str pos s' = case runParser parseString pos s' of
            Left err -> Left err
            Right (res, s'', pos') -> if str == res then Right (res, s'', pos') else Left (errorParsing pos)

-- | Return a Parser that parse a Bool (#f or #t)
parseBool :: Parser AST.Ast
parseBool = AST.Boolean <$> (parseTrue <|> parseFalse) <* parseWhiteSpace

-- | Return a PArser that parse a True (in lisp -> #t)
parseTrue :: Parser Bool
parseTrue = parseCmpString "true" >> return True

-- | Return a PArser that parse a True (in lisp -> #f)
parseFalse :: Parser Bool
parseFalse = parseCmpString "false" >> return False

parseDefineValue :: Parser AST.Ast
parseDefineValue = parseString
                        >>= \str -> parseChar '='
                            >> (parseAst >>= \ast -> return $ AST.Define str ast)

parseSymbol :: Parser AST.Ast
parseSymbol = do
                name <- parseString
                args <- optional (parseWhiteSpace *> parseList parseAst "(|" "|)"
                            >>= \res -> return $ AST.Symbol name (Just res))
                return $ fromMaybe (AST.Symbol name Nothing) args


parseDefineFn :: Parser AST.Ast
parseDefineFn = parseCmpString "fn" *> parseString
                    >>= \name -> parseFunctionValue
                        >>= \value -> return $ AST.Define name value

parseLambda :: Parser AST.Ast
parseLambda = (parseCmpString "lambda" <|> parseAnyString "λ") *> parseFunctionValue

parseFunctionValue :: Parser AST.Ast
-- parseFunctionValue = Parser parseParams
parseFunctionValue = parseList parseString "(|" "|)"
                        >>= \args -> parseBracket
                            >>= \expr -> return $ AST.FunctionValue args expr Nothing

parseBracket :: Parser AST.Ast
parseBracket = parseStart *> parseAst <* parseEnd
    where
        parseEnd = parseWhiteSpace *> parseAnyString "|}" <* parseWhiteSpace
        parseStart = parseWhiteSpace *> parseAnyString "{|" <* parseWhiteSpace

parseCond :: Parser AST.Ast
parseCond = do _ <- parseCmpString "if"
               expr <- parseExpr
               value <- parseBracket
               res <- optional parseElse
               return $ AST.Cond expr value res
    where
        parseElse = parseCmpString "else" *> Parser p
            where
                p :: Position -> String -> Either String (AST.Ast, String, Position)
                p pos s = case runParser parseCond pos s of
                    Left _ -> case runParser parseBracket pos s of
                        Left err -> Left err
                        Right (res, s', pos') -> Right (res, s', pos')
                    Right (res, s', pos') -> Right (res, s', pos')

-- | Return a Parser that parse a SExpr
parseAst :: Parser AST.Ast
parseAst = parseWhiteSpace *>
        (
        parseDefineFn
        <|> parseCond
        <|> parseDefineValue
        <|> parseLambda
        <|> parseBool
        <|> parseExpr
        <|> parseAstString
        <|> parseValue
        <|> parseSymbol
        )

parseComment :: Parser Char
parseComment = parseChar '#' *> Parser f
    where
        f :: Position -> String -> Either String (Char, String, Position)
        f (row, col) ('\n':xs)  = Right ('\n', xs, (row + 1, col))
        f (row, col) "" = Right ('\n', "", (row, col + 1))
        f (row, col) (_:xs) = f (row, col + 1) xs

parseLobster :: Parser [AST.Ast]
parseLobster = parseMany (parseWhiteSpace *> parseAst)
