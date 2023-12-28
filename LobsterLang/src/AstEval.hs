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
evalAst stack (Define s (FunctionValue params ast Nothing)) =
  (Nothing, addFuncToScope stack s params ast)
evalAst stack (Define s v) = (Nothing, addVarToScope stack s v)
evalAst stack (AST.Value i) = (Just (AST.Value i), stack)
evalAst stack (AST.Symbol s) =
  maybe
    (Nothing, stack)
    (evalAst stack)
    (getVarInScope stack s)
evalAst stack (Boolean b) = (Just (Boolean b), stack)
evalAst stack (Call "+" astList) = evalBinaryValueOp (+) stack (Call "+" astList)
evalAst stack (Call "-" astList) = evalBinaryValueOp (-) stack (Call "-" astList)
evalAst stack (Call "*" astList) = evalBinaryValueOp (*) stack (Call "*" astList)
evalAst stack (Call "/" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "/" astList) = evalBinaryValueOp (div) stack (Call "/" astList)
evalAst stack (Call "%" [_, AST.Value 0]) = (Nothing, stack)
evalAst stack (Call "%" astList) = evalBinaryValueOp (mod) stack (Call "%" astList)
evalAst stack (Call "==" astList) = evalBinaryCompareValueOp (==) stack (Call "==" astList)
evalAst stack (Call name params)
  | result == (Nothing, stack) = result
  | otherwise = Data.Bifunctor.second clearScope result
  where
    nsAndAst = maybe (stack, Nothing) (callFunc stack name)
      (evalSubParams stack params)
    result = maybe (Nothing, stack) (evalAst (fst nsAndAst)) (snd nsAndAst)
evalAst stack (FunctionValue _ _ Nothing) = (Nothing, stack)
evalAst stack (FunctionValue params ast (Just asts))
  | isNothing (fst result) = (Nothing, stack)
  | otherwise = Data.Bifunctor.second clearScope result
  where
    newStack = maybe stack (addVarsToScope (beginScope stack) params)
      (evalSubParams stack asts)
    result = evalAst newStack ast
evalAst stack (Cond (AST.Boolean b) a1 (Just a2))
  | b = evalAst stack a1
  | otherwise = evalAst stack a2
evalAst stack (Cond (AST.Boolean b) a1 Nothing)
  | b = evalAst stack a1
  | otherwise = (Nothing, stack)
evalAst stack (Cond ast a1 a2)
  | isNothing condEval = (Nothing, stack)
  | fromJust condEval == ast = (Nothing, stack)
  | otherwise = evalAst stack (Cond (fromJust condEval) a1 a2)
  where
    condEval = fst (evalAst stack ast)

evalBinaryValueOp :: (Int -> Int -> Int) -> [ScopeMb] -> Ast -> (Maybe Ast, [ScopeMb])
evalBinaryValueOp _ stack (Call _ [AST.Boolean _, _]) = (Nothing, stack)
evalBinaryValueOp _ stack (Call _ [_, AST.Boolean _]) = (Nothing, stack)
evalBinaryValueOp f stack  (Call _ [AST.Value a, AST.Value b]) =
  (Just (AST.Value (f a b)), stack)
evalBinaryValueOp _ stack  (Call op [ast1, ast2]) = maybe (Nothing, stack)
  (evalAst stack  . Call op) (evalSubParams stack [ast1, ast2])
evalBinaryValueOp _ stack (Call _ _) = (Nothing, stack)
evalBinaryValueOp _ stack _ = (Nothing, stack)

evalBinaryCompareValueOp :: (Int -> Int -> Bool) -> [ScopeMb] -> Ast -> (Maybe Ast, [ScopeMb])
evalBinaryCompareValueOp _ stack (Call _ [AST.Boolean _, _]) = (Nothing, stack)
evalBinaryCompareValueOp _ stack (Call _ [_, AST.Boolean _]) = (Nothing, stack)
evalBinaryCompareValueOp f stack  (Call _ [AST.Value a, AST.Value b]) =
  (Just (AST.Boolean (f a b)), stack)
evalBinaryCompareValueOp _ stack  (Call op [ast1, ast2]) = maybe (Nothing, stack)
  (evalAst stack  . Call op) (evalSubParams stack [ast1, ast2])
evalBinaryCompareValueOp _ stack (Call _ _) = (Nothing, stack)
evalBinaryCompareValueOp _ stack _ = (Nothing, stack)

evalSubParams :: [ScopeMb] -> [Ast] -> Maybe [Ast]
evalSubParams stack = mapM (fst . evalAst stack)
