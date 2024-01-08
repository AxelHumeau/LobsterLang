{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizer
-}

module AstOptimizer
  ( optimizeAst,
  )
where

import AST
import AstEval
import Data.Maybe
import Scope (ScopeMb)

data AstError = Error String Ast deriving (Eq, Show)

data AstOptimised
  = Result Ast
  | Warning String Ast
  deriving (Eq, Show)

optimizeAst :: [ScopeMb] -> [Ast] -> [Either AstError AstOptimised]
optimizeAst stack ((Value v) : xs) = Right (Result (Value v)) : optimizeAst stack xs
optimizeAst stack ((Boolean b) : xs) = Right (Result (Boolean b)) : optimizeAst stack xs
optimizeAst stack ((String str) : xs) = Right (Result (String str)) : optimizeAst stack xs
optimizeAst stack ((List asts) : xs) = case sequence (optimizeAst stack asts) of
  Left err -> Left err : optimizeAst stack xs
  Right opAst -> Right (Result (List (map fromOptimised opAst))) : optimizeAst stack xs
optimizeAst stack ((Define n ast) : xs) = case optimizeAst stack [ast] of
  [Left err] -> Left err : optimizeAst stack xs
  [Right (Result opAst)] -> Right (Result (Define n opAst)) : optimizeAst stack xs
  [Right (Warning mes opAst)] -> Right (Warning mes (Define n opAst)) : optimizeAst stack xs
  _ -> Right (Warning "This situation shouldn't happen" (Define n ast)) : optimizeAst stack xs
optimizeAst stack ((Symbol s Nothing) : xs) = Right (Result (Symbol s Nothing)) : optimizeAst stack xs
optimizeAst stack ((Symbol s (Just asts)) : xs)
  | foldr ((&&) . isUnoptimizable) True asts = case evalAst stack (Symbol s (Just asts)) of
      (Left err, _) -> Left (Error err (Symbol s (Just asts))) : optimizeAst stack xs
      (Right (Just _), stack') -> Right (Result (Symbol s (Just asts))) : optimizeAst stack' xs
      _ -> Right (Warning "This situation shouldn't happen" (Symbol s (Just asts))) : optimizeAst stack xs
  | otherwise = case sequence (optimizeAst stack asts) of
      Left err -> Left err : optimizeAst stack xs
      Right opAst -> Right (Result (Symbol s (Just (map fromOptimised opAst)))) : optimizeAst stack xs
optimizeAst stack ((Call op asts) : xs)
  | foldr ((&&) . isUnoptimizable) True asts
      && foldr ((&&) . isValue) True asts = case evalAst stack (Call op asts) of
      (Left err, _) -> Left (Error err (Call op asts)) : optimizeAst stack xs
      (Right (Just ast), stack') -> Right (Result ast) : optimizeAst stack' xs
      _ -> Right (Warning "This situation shouldn't happen" (Call op asts)) : optimizeAst stack xs
  | foldr ((&&) . isUnoptimizable) True asts = case evalAst stack (Call op asts) of
      (Left err, _) -> Left (Error err (Call op asts)) : optimizeAst stack xs
      (Right (Just _), stack') -> Right (Result (Call op asts)) : optimizeAst stack' xs
      _ -> Right (Warning "This situation shouldn't happen" (Call op asts)) : optimizeAst stack xs
  | otherwise = case sequence (optimizeAst stack asts) of
      Left err -> Left err : optimizeAst stack xs
      Right asts' -> optimizeAst stack (Call op (map fromOptimised asts') : xs)
optimizeAst stack ((Cond condAst trueAst mFalseAst) : xs)
  | not (isUnoptimizable condAst) = case optimizeAst stack [condAst] of
      [Left err] -> Left err : optimizeAst stack xs
      [Right (Result condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs)
      [Right (Warning _ condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs)
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs
  | not (isUnoptimizable trueAst) = case optimizeAst stack [trueAst] of
      [Left err] -> Left err : optimizeAst stack xs
      [Right (Result trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs)
      [Right (Warning _ trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs)
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs
  | isJust mFalseAst && not (isUnoptimizable (fromJust mFalseAst)) = case optimizeAst stack [fromJust mFalseAst] of
      [Left err] -> Left err : optimizeAst stack xs
      [Right (Result falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs)
      [Right (Warning _ falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs)
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs
  | otherwise = case condAst of
      Boolean True -> Right (Warning "Condition is always true" trueAst) : optimizeAst stack xs
      Boolean False ->
        Right
          ( Warning
              "Condition is always false"
              (fromMaybe (Cond condAst trueAst mFalseAst) mFalseAst)
          )
          : optimizeAst stack xs
      _ -> Right (Result (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs
optimizeAst stack ((FunctionValue params ast Nothing) : xs) = case optimizeAst stack [ast] of
  [Left err] -> Left err : optimizeAst stack xs
  [Right (Result ast')] -> Right (Result (FunctionValue params ast' Nothing)) : optimizeAst stack xs
  [Right (Warning mes ast')] -> Right (Warning mes (FunctionValue params ast' Nothing)) : optimizeAst stack xs
  _ -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast Nothing)) : optimizeAst stack xs
optimizeAst stack ((FunctionValue params ast (Just asts)) : xs)
  | not (isUnoptimizable ast) = case optimizeAst stack [ast] of
      [Left err] -> Left err : optimizeAst stack xs
      [Right (Result ast')] -> optimizeAst stack (FunctionValue params ast' Nothing : xs)
      [Right (Warning _ ast')] -> optimizeAst stack (FunctionValue params ast' Nothing : xs)
      _ -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast (Just asts))) : optimizeAst stack xs
  | not (foldr ((&&) . isUnoptimizable) True asts) = case sequence (optimizeAst stack asts) of
      Left err -> Left err : optimizeAst stack xs
      Right asts' -> optimizeAst stack (FunctionValue params ast (Just (map fromOptimised asts')) : xs)
  | length params > length asts = case evalAst stack (FunctionValue params ast (Just asts)) of
      (Left err, _) -> Left (Error err (FunctionValue params ast (Just asts))) : optimizeAst stack xs
      (Right (Just ast'), stack') -> Right (Result ast') : optimizeAst stack' xs
      (Right Nothing, _) -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast (Just asts))) : optimizeAst stack xs
  | otherwise = Right (Result (FunctionValue params ast (Just asts))) : optimizeAst stack xs
optimizeAst _ [] = []

isUnoptimizable :: Ast -> Bool
isUnoptimizable (Define _ ast) = isUnoptimizable ast
isUnoptimizable (Value _) = True
isUnoptimizable (Boolean _) = True
isUnoptimizable (String _) = True
isUnoptimizable (List asts) = foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (Call _ asts) =
  foldr ((&&) . isUnoptimizable) True asts
    && not (foldr ((&&) . isValue) True asts)
isUnoptimizable (Symbol _ Nothing) = True
isUnoptimizable (Symbol _ (Just asts)) = foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (FunctionValue _ ast Nothing) = isUnoptimizable ast
isUnoptimizable (FunctionValue params ast (Just asts)) =
  isUnoptimizable ast
    && foldr ((&&) . isUnoptimizable) True asts
    && length params > length asts
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

fromOptimised :: AstOptimised -> Ast
fromOptimised (Warning _ ast) = ast
fromOptimised (Result ast) = ast
