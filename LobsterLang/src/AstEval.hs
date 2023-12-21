{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AstEval
-}

module AstEval (sexprToAst, evalAst) where

import AST
import qualified Data.Bifunctor
import Data.Maybe
import SExpr
import Scope

-- | Convert a S-expression into an 'Ast',
-- return Nothing if the expression is invalid or Just the Ast
sexprToAst :: SExpr -> Maybe Ast
sexprToAst (SExpr.List [SExpr.Symbol "define", SExpr.Symbol s, t]) =
  Define s <$> sexprToAst t
sexprToAst (SExpr.List (SExpr.Symbol f : xs)) =
  Call f <$> mapM sexprToAst xs
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
evalAst stack (Define s (FunctionValue params ast Nothing)) = (Nothing, addFuncToScope stack s params ast)
evalAst stack (Define s v) = (Nothing, addVarToScope stack s v)
evalAst stack (AST.Value i) = (Just (AST.Value i), stack)
evalAst stack (AST.Symbol s) =
  maybe
    (Nothing, stack)
    (evalAst stack)
    (getVarInScope stack s)
evalAst stack (Boolean b) = (Just (Boolean b), stack)
evalAst stack (Call "+" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "+" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "+" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (a + b)), stack)
evalAst stack (Call "+" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "+") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "+" _) = (Nothing, stack)
evalAst stack (Call "-" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "-" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "-" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (a - b)), stack)
evalAst stack (Call "-" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "-") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "-" _) = (Nothing, stack)
evalAst stack (Call "*" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "*" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "*" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (a * b)), stack)
evalAst stack (Call "*" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "*") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "*" _) = (Nothing, stack)
evalAst stack (Call "/" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "/" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "/" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "/" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (div a b)), stack)
evalAst stack (Call "/" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "/") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "/" _) = (Nothing, stack)
evalAst stack (Call "%" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "%" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "%" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "%" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (mod a b)), stack)
evalAst stack (Call "%" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "%") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "%" _) = (Nothing, stack)
evalAst stack (Call "==" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "==" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "==" [AST.Value a, AST.Value b]) =
  (Just (AST.Boolean (a == b)), stack)
evalAst stack (Call "==" [ast1, ast2]) = maybe (Nothing, stack) (evalAst stack . Call "==") (evalSubParams stack [ast1, ast2])
evalAst stack (Call "==" _) = (Nothing, stack)
evalAst stack (Call name params)
  | result == (Nothing, stack) = result
  | otherwise = Data.Bifunctor.second clearScope result
  where
    nsAndAst = callFunc stack name params
    result = maybe (Nothing, stack) (evalAst (fst nsAndAst)) (snd nsAndAst)
evalAst stack (FunctionValue _ _ Nothing) = (Nothing, stack)
evalAst stack (FunctionValue params ast (Just asts))
  | isNothing (fst result) = (Nothing, stack)
  | otherwise = Data.Bifunctor.second clearScope result
  where
    newStack = addVarsToScope (beginScope stack) params asts
    result = evalAst newStack ast
evalAst stack (Cond (AST.Boolean b) a1 (Just a2))
  | b = evalAst stack a1
  | otherwise = evalAst stack a2
evalAst stack (Cond (AST.Boolean b) a1 Nothing)
  | b = evalAst stack a1
  | otherwise = (Nothing, stack)
evalAst stack _ = (Nothing, stack)

evalSubParams :: [ScopeMb] -> [Ast] -> Maybe [Ast]
evalSubParams stack = mapM (fst . evalAst stack)
