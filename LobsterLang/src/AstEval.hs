{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AstEval
-}

module AstEval (sexprToAst, evalAst) where

import AST
import qualified Data.Bifunctor
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
evalAst stack (Call "+" [ast1,ast2]) = maybe (Nothing, stack) (evalAst stack . Call "+") (evalSubParams stack [ast1,ast2])
evalAst stack (Call "+" _) = (Nothing, stack)

evalAst stack (Call "-" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "-" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "-" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (a - b)), stack)
evalAst stack (Call "-" [ast1,ast2]) = maybe (Nothing, stack) (evalAst stack . Call "-") (evalSubParams stack [ast1,ast2])
evalAst stack (Call "-" _) = (Nothing, stack)

evalAst stack (Call "*" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "*" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "*" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (a * b)), stack)
evalAst stack (Call "*" [ast1,ast2]) = maybe (Nothing, stack) (evalAst stack . Call "*") (evalSubParams stack [ast1,ast2])
evalAst stack (Call "*" _) = (Nothing, stack)

evalAst stack (Call "/" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "/" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "/" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "/" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (div a b)), stack)
evalAst stack (Call "/" [ast1,ast2]) = maybe (Nothing, stack) (evalAst stack . Call "/") (evalSubParams stack [ast1,ast2])
evalAst stack (Call "/" _) = (Nothing, stack)

evalAst stack (Call "%" [AST.Boolean _, _]) = (Nothing, stack)
evalAst stack (Call "%" [_, AST.Boolean _]) = (Nothing, stack)
evalAst stack (Call "%" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "%" [AST.Value a, AST.Value b]) =
  (Just (AST.Value (mod a b)), stack)
evalAst stack (Call "%" [ast1,ast2]) = maybe (Nothing, stack) (evalAst stack . Call "%") (evalSubParams stack [ast1,ast2])
evalAst stack (Call "%" _) = (Nothing, stack)

evalAst stack (Call name params)
  | result == (Nothing, stack) = result
  | otherwise = Data.Bifunctor.second clearScope result
  where
    nsAndAst = callFunc stack name params
    result = maybe (Nothing, stack) (evalAst (fst nsAndAst)) (snd nsAndAst)

evalSubParams :: [ScopeMb] -> [Ast] -> Maybe [Ast]
evalSubParams stack = mapM (fst . evalAst stack)
-- evalSubParams stack (Call c [ast1, ast2])
--   | eval1 == Just ast1 && eval2 == Just ast2 = (Nothing, stack)
--   | otherwise = maybe (Nothing, stack) (evalAst stack . Call c) (sequence [eval1, eval2])
--   where
--     eval1 = fst (evalAst stack ast1)
--     eval2 = fst (evalAst stack ast2)
-- evalSubParams stack _ = (Nothing, stack)
