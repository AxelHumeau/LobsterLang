{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AstEval
-}

module AstEval(sexprToAst, evalAst) where

import SExpr
import AST
import Scope

-- | Convert a S-expression into an 'Ast',
-- return Nothing if the expression is invalid or Just the Ast
sexprToAst :: SExpr -> Maybe Ast
sexprToAst (SExpr.List [(SExpr.Symbol "define"), (SExpr.Symbol s), t]) =
        (Define s) <$> sexprToAst t
sexprToAst (SExpr.List (SExpr.Symbol f:xs)) =
        (Call f) <$> sequence (map sexprToAst xs)
sexprToAst (SExpr.List _) = Nothing
sexprToAst (SExpr.Value i) = Just (AST.Value i)
sexprToAst (SExpr.Symbol "true") = Just (Boolean True)
sexprToAst (SExpr.Symbol "false") = Just (Boolean False)
sexprToAst (SExpr.Symbol s) = Just (AST.Symbol s)

-- | Evaluate a 'Ast'.
-- Takes a stack representing variables and the Ast to evaluate.
-- Returns a tuple containing the resulting Ast
-- (or Nothing if an error occured or a non evaluable Ast is given)
-- and the stack after evaluation.
evalAst :: [ScopeMb] -> Ast -> (Maybe Ast, [ScopeMb])
evalAst stack (Define s v) = (Nothing, addVarToScope stack s v)
evalAst stack (AST.Value i) = (Just (AST.Value i), stack)
evalAst stack (AST.Symbol s) = result
        where result = maybe (Nothing, stack) (evalAst stack)
                (getVarInScope stack s)
evalAst stack (Boolean b) = (Just (Boolean b), stack)
evalAst stack (Call "+" [AST.Value a, AST.Value b]) =
        (Just (AST.Value (a + b)), stack)
evalAst stack (Call "+" [AST.Symbol s1, AST.Symbol s2]) =
        (Just (AST.Symbol (s1 ++ s2)), stack)
evalAst stack (Call "-" [AST.Value a, AST.Value b]) =
        (Just (AST.Value (a - b)), stack)
evalAst stack (Call "*" [AST.Value a, AST.Value b]) =
        (Just (AST.Value (a * b)), stack)
evalAst stack (Call "/" [AST.Value a, AST.Value b]) =
        (Just (AST.Value (div a b)), stack)
evalAst stack (Call "%" [AST.Value a, AST.Value b]) =
        (Just (AST.Value (mod a b)), stack)
evalAst stack (Call c [t1, t2])
    | evalt1 == Just t1 && evalt2 == Just t2 = (Nothing, stack)
    | c `elem` ["+", "-", "/", "*", "%"] =
        maybe (Nothing, stack) (evalAst stack)
        ((Call c) <$>
        (sequence [fst (evalAst stack t1), fst (evalAst stack t2)]))
    | otherwise = (Nothing, stack)
    where evalt1 = fst (evalAst stack t1)
          evalt2 = fst (evalAst stack t2)
evalAst stack _ = (Nothing, stack)
