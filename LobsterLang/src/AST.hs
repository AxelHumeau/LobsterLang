module AST (Ast) where

import SExpr

data Ast = Define String Ast
        | Integer Int
        | Symbol String
        | Boolean Bool
        | Call String [Ast]
        deriving Show

sexprToAst :: SExpr -> Maybe Ast
sexprToAst (SExpr.List [(SExpr.Symbol "define"), (SExpr.Symbol s), t]) = (Define s) <$> sexprToAst t
sexprToAst (SExpr.List (SExpr.Symbol f:xs)) = (Call f) <$> sequence (map sexprToAst xs)
sexprToAst (SExpr.List _) = Nothing
sexprToAst (SExpr.Integer i) = Just (AST.Integer i)
sexprToAst (SExpr.Symbol "true") = Just (Boolean True)
sexprToAst (SExpr.Symbol "false") = Just (Boolean False)
sexprToAst (SExpr.Symbol s) = Just (AST.Symbol s)

