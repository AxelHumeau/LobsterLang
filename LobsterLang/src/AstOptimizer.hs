{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizer
-}

module AstOptimizer
  ( optimizeAst,
    AstError(..),
    AstOptimised(..),
  )
where

import AST
import AstEval
import Data.Maybe
import Scope (ScopeMb, getVarInScope)

data AstError = Error String Ast deriving (Eq, Show)

data AstOptimised
  = Result Ast
  | Warning String Ast
  deriving (Eq, Show)

optimizeAst :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
optimizeAst stack ((Value v) : xs) inFunc = Right (Result (Value v)) : optimizeAst stack xs inFunc
optimizeAst stack ((Boolean b) : xs) inFunc = Right (Result (Boolean b)) : optimizeAst stack xs inFunc
optimizeAst stack ((String str) : xs) inFunc = Right (Result (String str)) : optimizeAst stack xs inFunc
optimizeAst stack ((List asts) : xs) inFunc = case sequence (optimizeAst stack asts inFunc) of
  Left err -> Left err : optimizeAst stack xs inFunc
  Right opAst -> Right (Result (List (map fromOptimised opAst))) : optimizeAst stack xs inFunc
optimizeAst stack ((Define n ast) : xs) inFunc = case optimizeAst stack [ast] inFunc of
  [Left err] -> Left err : optimizeAst stack xs inFunc
  [Right (Result opAst)] -> case evalAst stack (Define n opAst) of
    (Right _, stack') -> Right (Result (Define n opAst)) : optimizeAst stack' xs inFunc
    (Left ('S':'y':'m':'b':'o':'l':' ':'\'':xs'), _)
      | inFunc -> Right (Result (Define n opAst)) : optimizeAst stack xs inFunc
      | otherwise -> Left (Error ('S':'y':'m':'b':'o':'l':' ':'\'':xs') (Define n opAst)) : optimizeAst stack xs inFunc
    (Left err, _) -> Left (Error err (Define n opAst)) : optimizeAst stack xs inFunc
  [Right (Warning mes opAst)] -> Right (Warning mes (Define n opAst)) : optimizeAst stack xs inFunc
  _ -> Right (Warning "This situation shouldn't happen" (Define n ast)) : optimizeAst stack xs inFunc
optimizeAst stack ((Symbol s Nothing) : xs) inFunc
  | inFunc = Right (Result (Symbol s Nothing)) : optimizeAst stack xs inFunc
  | otherwise = case getVarInScope stack s of
    Nothing -> Left (Error ("Symbol '" ++ s ++ "' doesn't exist in the current or global scope") (Symbol s Nothing)) : optimizeAst stack xs inFunc
    Just _ -> Right (Result (Symbol s Nothing)) : optimizeAst stack xs inFunc
optimizeAst stack ((Symbol s (Just asts)) : xs) inFunc
  | foldr ((&&) . isUnoptimizable) True asts = case evalAst stack (Symbol s (Just asts)) of
      (Left ('S':'y':'m':'b':'o':'l':' ':'\'':xs'), _)
        | inFunc -> Right (Result (Symbol s (Just asts))) : optimizeAst stack xs inFunc
        | otherwise -> Left (Error ('S':'y':'m':'b':'o':'l':' ':'\'':xs') (Symbol s (Just asts))) : optimizeAst stack xs inFunc
      (Left err, _) -> Left (Error err (Symbol s (Just asts))) : optimizeAst stack xs inFunc
      (Right (Just _), stack') -> Right (Result (Symbol s (Just asts))) : optimizeAst stack' xs inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Symbol s (Just asts))) : optimizeAst stack xs inFunc
  | otherwise = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right opAst -> optimizeAst stack (Symbol s (Just (map fromOptimised opAst)):xs) inFunc
optimizeAst stack ((Call op asts) : xs) inFunc
  | foldr ((&&) . isUnoptimizable) True asts
      && foldr ((&&) . isValue) True asts = case evalAst stack (Call op asts) of
      (Left ('S':'y':'m':'b':'o':'l':' ':'\'':xs'), _)
        | inFunc -> Right (Result (Call op asts)) : optimizeAst stack xs inFunc
        | otherwise -> Left (Error ('S':'y':'m':'b':'o':'l':' ':'\'':xs') (Call op asts)) : optimizeAst stack xs inFunc
      (Left err, _) -> Left (Error err (Call op asts)) : optimizeAst stack xs inFunc
      (Right (Just ast), stack') -> Right (Result ast) : optimizeAst stack' xs inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Call op asts)) : optimizeAst stack xs inFunc
  | foldr ((&&) . isUnoptimizable) True asts = case evalAst stack (Call op asts) of
      (Left ('S':'y':'m':'b':'o':'l':' ':'\'':xs'), _)
        | inFunc -> Right (Result (Call op asts))  : optimizeAst stack xs inFunc
        | otherwise -> Left (Error ('S':'y':'m':'b':'o':'l':' ':'\'':xs') (Call op asts)) : optimizeAst stack xs inFunc
      (Left err, _) -> Left (Error err (Call op asts)) : optimizeAst stack xs inFunc
      (Right (Just _), stack') -> Right (Result (Call op asts)) : optimizeAst stack' xs inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Call op asts)) : optimizeAst stack xs inFunc
  | otherwise = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right asts' -> optimizeAst stack (Call op (map fromOptimised asts') : xs) inFunc
optimizeAst stack ((Cond condAst trueAst mFalseAst) : xs) inFunc
  | not (isUnoptimizable condAst) = case optimizeAst stack [condAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inFunc
      [Right (Warning _ condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs inFunc
  | not (isUnoptimizable trueAst) = case optimizeAst stack [trueAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inFunc
      [Right (Warning _ trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs inFunc
  | isJust mFalseAst && not (isUnoptimizable (fromJust mFalseAst)) = case optimizeAst stack [fromJust mFalseAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inFunc
      [Right (Warning _ falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inFunc
      _ -> Right (Warning "This situation shouldn't happen" (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs inFunc
  | otherwise = case condAst of
      Boolean True -> Right (Warning "Condition is always true" trueAst) : optimizeAst stack xs inFunc
      Boolean False ->
        Right
          ( Warning
              "Condition is always false"
              (fromMaybe (Cond condAst trueAst mFalseAst) mFalseAst)
          )
          : optimizeAst stack xs inFunc
      _ -> Right (Result (Cond condAst trueAst mFalseAst)) : optimizeAst stack xs inFunc
optimizeAst stack ((FunctionValue params ast Nothing) : xs) inFunc = case optimizeAst stack [ast] True of
  [Left err] -> Left err : optimizeAst stack xs inFunc
  [Right (Result ast')] -> Right (Result (FunctionValue params ast' Nothing)) : optimizeAst stack xs inFunc
  [Right (Warning mes ast')] -> Right (Warning mes (FunctionValue params ast' Nothing)) : optimizeAst stack xs inFunc
  _ -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast Nothing)) : optimizeAst stack xs inFunc
optimizeAst stack ((FunctionValue params ast (Just asts)) : xs) inFunc
  | not (isUnoptimizable ast) = case optimizeAst stack [ast] True of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result ast')] -> optimizeAst stack (FunctionValue params ast' Nothing : xs) inFunc
      [Right (Warning _ ast')] -> optimizeAst stack (FunctionValue params ast' Nothing : xs) inFunc
      _ -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast (Just asts))) : optimizeAst stack xs inFunc
  | not (foldr ((&&) . isUnoptimizable) True asts) = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right asts' -> optimizeAst stack (FunctionValue params ast (Just (map fromOptimised asts')) : xs) inFunc
  | length params > length asts = case evalAst stack (FunctionValue params ast (Just asts)) of
      (Left err, _) -> Left (Error err (FunctionValue params ast (Just asts))) : optimizeAst stack xs inFunc
      (Right (Just ast'), stack') -> Right (Result ast') : optimizeAst stack' xs inFunc
      (Right Nothing, _) -> Right (Warning "This situation shouldn't happen" (FunctionValue params ast (Just asts))) : optimizeAst stack xs inFunc
  | otherwise = Right (Result (FunctionValue params ast (Just asts))) : optimizeAst stack xs inFunc
optimizeAst _ [] _ = []

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
