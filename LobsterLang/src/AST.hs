module AST (Ast(..), sexprToAst, evalAst) where

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

evalAst :: Ast -> Maybe Ast
evalAst (Define _ _) = Nothing
evalAst (AST.Integer i) = Just (AST.Integer i)
evalAst (AST.Symbol s) = Just (AST.Symbol s)
evalAst (Boolean b) = Just (Boolean b)
evalAst (Call "+" [AST.Integer a, AST.Integer b]) = Just (AST.Integer (a + b))
evalAst (Call "+" [AST.Symbol s1, AST.Symbol s2]) = Just (AST.Symbol (s1 ++ s2))
evalAst (Call "-" [AST.Integer a, AST.Integer b]) = Just (AST.Integer (a - b))
evalAst (Call "*" [AST.Integer a, AST.Integer b]) = Just (AST.Integer (a * b))
evalAst (Call "/" [AST.Integer a, AST.Integer b]) = Just (AST.Integer (div a b))
evalAst (Call "%" [AST.Integer a, AST.Integer b]) = Just (AST.Integer (mod a b))
evalAst (Call _ [AST.Symbol _, _]) = Nothing
evalAst (Call _ [_, AST.Symbol _]) = Nothing
evalAst (Call _ [Boolean _, _]) = Nothing
evalAst (Call _ [_, Boolean _]) = Nothing
evalAst (Call c [t1, t2]) = maybe Nothing evalAst ((Call c) <$> (sequence [evalAst t1, evalAst t2]))
evalAst _ = Nothing
