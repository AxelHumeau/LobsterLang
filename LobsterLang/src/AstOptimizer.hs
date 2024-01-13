{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizer
-}

module AstOptimizer
  ( optimizeAst,
    fromOptimised,
    AstError (..),
    AstOptimised (..),
  )
where

import AST
import AstEval
import Data.Maybe
import Scope (ScopeMb, getVarInScope)

-- Represent an error containing the error message
-- and the `Ast` that caused it
data AstError = Error String Ast deriving (Eq, Show)

-- Represent an AST after optimization
data AstOptimised
  =
  -- | The `Ast` after optimization
  Result Ast
  -- | When the optimization throw a warning
  -- contains the warining message and the `Ast`
  -- post optimization that caused it
  | Warning String Ast
  deriving (Eq, Show)

-- | Optimize a list of `Ast` and check for invalid operation:
-- optimization is taking place when operation have the same result no matter what
-- for exemple `3 + 3`, when a forbidden operation is taking place, the
-- optimization results in an `AstError`, when the optimization was a success
-- it results in an `AstOptimised`.
-- Takes the stack (`[ScopeMb]`), a list of `Ast`, a boolean to indicate whether
-- the optimization take place insinde a function and returns the list of `Either`
-- `AstError` or `AstOptimised`
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
    (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
      | inFunc -> Right (Result (Define n opAst)) : optimizeAst stack xs inFunc
      | otherwise -> Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') (Define n opAst)) : optimizeAst stack xs inFunc
    (Left err, _) -> Left (Error err (Define n opAst)) : optimizeAst stack xs inFunc
  [Right (Warning mes opAst)] -> case evalAst stack (Define n opAst) of
    (Right _, stack') -> Right (Warning mes (Define n opAst)) : optimizeAst stack' xs inFunc
    (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
      | inFunc -> Right (Result (Define n opAst)) : optimizeAst stack xs inFunc
      | otherwise -> Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') (Define n opAst)) : optimizeAst stack xs inFunc
    (Left err, _) -> Left (Error err (Define n opAst)) : optimizeAst stack xs inFunc
  _ -> shouldntHappen stack (Define n ast : xs) inFunc
optimizeAst stack ((Symbol s Nothing) : xs) inFunc
  | inFunc = Right (Result (Symbol s Nothing)) : optimizeAst stack xs inFunc
  | otherwise = case getVarInScope stack s of
      Nothing -> Left (Error ("Symbol '" ++ s ++ "' doesn't exist in the current or global scope") (Symbol s Nothing)) : optimizeAst stack xs inFunc
      Just _ -> Right (Result (Symbol s Nothing)) : optimizeAst stack xs inFunc
optimizeAst stack ((Symbol s (Just asts)) : xs) inFunc
  | foldr ((&&) . isUnoptimizable) True asts = checkEvalReturnSame stack (Symbol s (Just asts) : xs) inFunc
  | otherwise = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right opAst -> optimizeAst stack (Symbol s (Just (map fromOptimised opAst)) : xs) inFunc
optimizeAst stack ((Call op asts) : xs) inFunc
  | foldr ((&&) . isUnoptimizable) True asts
      && foldr ((&&) . isValue) True asts =
      checkEval stack (Call op asts : xs) inFunc
  | foldr ((&&) . isUnoptimizable) True asts = checkEvalReturnSame stack (Call op asts : xs) inFunc
  | otherwise = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right asts' -> optimizeAst stack (Call op (map fromOptimised asts') : xs) inFunc
optimizeAst stack ((Cond condAst trueAst mFalseAst) : xs) inFunc
  | not (isUnoptimizable condAst) = case optimizeAst stack [condAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inFunc
      [Right (Warning _ condAst')] -> optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inFunc
      _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inFunc
  | not (isUnoptimizable trueAst) = case optimizeAst stack [trueAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inFunc
      [Right (Warning _ trueAst')] -> optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inFunc
      _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inFunc
  | isJust mFalseAst && not (isUnoptimizable (fromJust mFalseAst)) = case optimizeAst stack [fromJust mFalseAst] inFunc of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inFunc
      [Right (Warning _ falseAst')] -> optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inFunc
      _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inFunc
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
optimizeAst stack (FunctionValue params ast Nothing : xs) inFunc = case optimizeAst stack [ast] True of
  [Left err] -> Left err : optimizeAst stack xs inFunc
  [Right (Result ast')] -> Right (Result (FunctionValue params ast' Nothing)) : optimizeAst stack xs inFunc
  [Right (Warning mes ast')] -> Right (Warning mes (FunctionValue params ast' Nothing)) : optimizeAst stack xs inFunc
  _ -> shouldntHappen stack (FunctionValue params ast Nothing : xs) inFunc
optimizeAst stack (FunctionValue params ast (Just asts) : xs) inFunc
  | not (isUnoptimizable ast) = case optimizeAst stack [ast] True of
      [Left err] -> Left err : optimizeAst stack xs inFunc
      [Right (Result ast')] -> optimizeAst stack (FunctionValue params ast' (Just asts) : xs) inFunc
      [Right (Warning _ ast')] -> optimizeAst stack (FunctionValue params ast' (Just asts) : xs) inFunc
      _ -> shouldntHappen stack (FunctionValue params ast (Just asts) : xs) inFunc
  | not (foldr ((&&) . isUnoptimizable) True asts) = case sequence (optimizeAst stack asts inFunc) of
      Left err -> Left err : optimizeAst stack xs inFunc
      Right asts' -> optimizeAst stack (FunctionValue params ast (Just (map fromOptimised asts')) : xs) inFunc
  | length params > length asts = case evalAst stack (FunctionValue params ast (Just asts)) of
      (Left err, _) -> Left (Error err (FunctionValue params ast (Just asts))) : optimizeAst stack xs inFunc
      (Right (Just ast'), stack') -> Right (Result ast') : optimizeAst stack' xs inFunc
      (Right Nothing, _) -> shouldntHappen stack (FunctionValue params ast (Just asts) : xs) inFunc
  | otherwise = checkEvalReturnSame stack (FunctionValue params ast (Just asts) : xs) inFunc
optimizeAst _ [] _ = []

-- | Check whether an `Ast` is optimizable
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

-- | Check whether the `Ast` is a constant value
isValue :: Ast -> Bool
isValue (Value _) = True
isValue (Boolean _) = True
isValue (String _) = True
isValue (List _) = True
isValue (FunctionValue _ _ Nothing) = True
isValue _ = False

-- | Get the `Ast` contained in a `AstOptimised`
fromOptimised :: AstOptimised -> Ast
fromOptimised (Warning _ ast) = ast
fromOptimised (Result ast) = ast

-- | Handle cases where the optimization depends on
-- the result of a evaluation of the `Ast` and it have to return evaluated
-- result
checkEval :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
checkEval stack (ast : xs) inFunc = case evalAst stack ast of
  (Left ('R' : 'e' : 'c' : 'u' : 'r' : 's' : 'i' : 'o' : 'n' : _), _) ->
    Right (Warning "Possible infinite recursion" ast) : optimizeAst stack xs inFunc
  (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
    | inFunc -> Right (Result ast) : optimizeAst stack xs inFunc
    | otherwise -> Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') ast) : optimizeAst stack xs inFunc
  (Left err, _) -> Left (Error err ast) : optimizeAst stack xs inFunc
  (Right (Just ast'), stack') -> Right (Result ast') : optimizeAst stack' xs inFunc
  _ -> shouldntHappen stack (ast : xs) inFunc
checkEval _ _ _ = [Right (Warning "This situation really shouldn't happen" (String "bruh"))]

-- | Handle cases where the optimization depends on
-- the result of a evaluation of the `Ast` and it have to return the original `Ast`
checkEvalReturnSame :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
checkEvalReturnSame stack (ast : xs) inFunc = case evalAst stack ast of
  (Left ('R' : 'e' : 'c' : 'u' : 'r' : 's' : 'i' : 'o' : 'n' : _), _) ->
    Right (Warning "Possible infinite recursion" ast) : optimizeAst stack xs inFunc
  (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
    | inFunc -> Right (Result ast) : optimizeAst stack xs inFunc
    | otherwise -> Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') ast) : optimizeAst stack xs inFunc
  (Left err, _) -> Left (Error err ast) : optimizeAst stack xs inFunc
  (Right (Just _), stack') -> Right (Result ast) : optimizeAst stack' xs inFunc
  _ -> shouldntHappen stack (ast : xs) inFunc
checkEvalReturnSame _ _ _ = [Right (Warning "This situation really shouldn't happen" (String "bruh"))]

shouldntHappen :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
shouldntHappen stack (ast : xs) inFunc =
  Right (Warning "This situation shouldn't happen" ast) : optimizeAst stack xs inFunc
shouldntHappen _ _ _ = [Right (Warning "This situation really shouldn't happen" (String "bruh"))]
