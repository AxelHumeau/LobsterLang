{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- SExpr
-}

module SExpr (SExpr(..), getSymbol, getInteger, getList, printTree) where

-- | S-Expression
data SExpr = Value Int
            | Symbol String
            | List [SExpr]
    deriving(Show, Eq)

-- instance Show SExpr where
--     show (Value i) = show i
--     show (Symbol s) = show s
--     show (List l) = filter (/= '\"')
--         ("( " ++ foldr (++) "" ((++ " ") <$> (show <$> l)) ++ ")")

-- | Get the 'Symbol' contained in this expression
getSymbol :: SExpr -> Maybe String
getSymbol (Value _) = Nothing
getSymbol (List _) = Nothing
getSymbol (Symbol s) = Just s

-- | Get the 'Integer' contained in this expression
getInteger :: SExpr -> Maybe Int
getInteger (Value int) = Just int
getInteger (List _) = Nothing
getInteger (Symbol _) = Nothing

-- | Get the 'List' contained in this expression
getList :: SExpr -> Maybe [SExpr]
getList (Value _) = Nothing
getList (List l) = Just l
getList (Symbol _) = Nothing

-- | Return a string representation of the S-Expression
printTree :: SExpr -> Maybe String
printTree s = Just (show s)
