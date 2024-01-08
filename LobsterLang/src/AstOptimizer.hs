{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizer
-}

module AstOptimizer where

import AST
import Scope (ScopeMb)
import AstEval

data AstError = Error String Ast
              | Warning String Ast

optimizeAst :: [ScopeMb] -> [Ast] -> [Either AstError Ast]
optimizeAst stack ((Define n ast):xs) = case optimizeAst stack [ast] of
    [Left err] -> Left err : optimizeAst stack xs
    [Right opAst] -> Right (Define n opAst) : optimizeAst stack xs
    _ -> Left (Warning "This situation shouldn't happen" (Define n ast)) : optimizeAst stack xs
optimizeAst stack ((Call op asts):xs)
    | foldr ((&&) . isUnoptimizable) True asts &&
    foldr ((&&) . isValue) True asts = case evalAst stack (Call op asts) of
        (Left err, _) -> Left (Error err (Call op asts)) : optimizeAst stack xs
        (Right (Just ast), stack') -> Right ast : optimizeAst stack' xs
        _ -> Left (Warning "This situation shouldn't happen" (Call op asts)) : optimizeAst stack xs
    | foldr ((&&) . isUnoptimizable) True asts = Right (Call op asts) : optimizeAst stack xs
    | otherwise = case sequence (optimizeAst stack asts) of
        Left err -> Left err : optimizeAst stack xs
        Right asts' -> optimizeAst stack (Call op asts':xs)
optimizeAst stack (ast:xs) = Right ast : optimizeAst stack xs
optimizeAst _ [] = []

isUnoptimizable :: Ast -> Bool
isUnoptimizable (Define _ ast) = isUnoptimizable ast
isUnoptimizable (Value _) = True
isUnoptimizable (Boolean _) = True
isUnoptimizable (String _) = True
isUnoptimizable (List asts) = foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (Call _ asts) = foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (Symbol _ Nothing) = True
isUnoptimizable (Symbol _ (Just asts)) = foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (FunctionValue _ ast Nothing) = isUnoptimizable ast
isUnoptimizable (FunctionValue params ast (Just asts)) =
    isUnoptimizable ast &&
    foldr ((&&) . isUnoptimizable) True asts &&
    length params > length asts
isUnoptimizable (Cond (Boolean _) _ _) = False
isUnoptimizable (Cond condAst bodyAst Nothing) =
    isUnoptimizable condAst && isUnoptimizable bodyAst
isUnoptimizable (Cond condAst bodyAst (Just elseAst)) =
    isUnoptimizable condAst && isUnoptimizable bodyAst && isUnoptimizable elseAst

isValue :: Ast -> Bool
isValue (Value _) = True
isValue (Boolean _) = True
isValue (String _) = True
isValue (List _) = True
isValue (FunctionValue _ _ Nothing) = True
isValue _ = False
