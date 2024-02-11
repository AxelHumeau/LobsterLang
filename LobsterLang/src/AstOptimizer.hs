{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizer
-}

module AstOptimizer
  ( optimizeAst,
    fromOpti,
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
  = -- | The `Ast` after optimization
    Result Ast
  | -- | When the optimization throw a warning
    -- contains the warining message and the `Ast`
    -- post optimization that caused it
    Warning String Ast
  deriving (Eq, Show)

-- | Optimize a list of `Ast` and check for invalid operation:
-- optimization is taking place when operation have the same result no matter
-- what for exemple `3 + 3`, when a forbidden operation is taking place, the
-- optimization results in an `AstError`, when the optimization was a success
-- it results in an `AstOptimised`.
-- Takes the stack (`[ScopeMb]`), a list of `Ast`, a boolean to indicate
--  whether the optimization take place insinde a function and returns the
-- list of `Either` `AstError` or `AstOptimised`
optimizeAst :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
optimizeAst stack ((Value v) : xs) inF =
  Right (Result (Value v)) : optimizeAst stack xs inF
optimizeAst stack ((Boolean b) : xs) inF =
  Right (Result (Boolean b)) : optimizeAst stack xs inF
optimizeAst stack ((String str) : xs) inF =
  Right (Result (String str)) : optimizeAst stack xs inF
optimizeAst stack ((List asts) : xs) inF =
  case sequence (optimizeAst stack asts inF) of
    Left err -> Left err : optimizeAst stack xs inF
    Right opAst ->
      Right (Result (List (map fromOpti opAst)))
        : optimizeAst stack xs inF
optimizeAst stack ((Define n ast) : xs) inF =
  checkOptiAfterDef stack (optimizeAst stack [ast] inF) n ast xs inF
optimizeAst stack ((Symbol s Nothing) : xs) inF
  | inF = Right (Result (Symbol s Nothing)) : optimizeAst stack xs inF
  | otherwise = case getVarInScope stack s of
      Nothing -> Left (Error ("Symbol '" ++ s ++
        "' doesn't exist in the current or global scope") (Symbol s Nothing))
          : optimizeAst stack xs inF
      Just _ -> Right (Result (Symbol s Nothing)) : optimizeAst stack xs inF
optimizeAst stack ((Symbol s (Just asts)) : xs) inF
  | foldr ((&&) . isUnoptimizable) True asts =
      checkEvalReturnSame stack (Symbol s (Just asts) : xs)
      (evalAst stack (Symbol s (Just asts))) inF
  | otherwise = case sequence (optimizeAst stack asts inF) of
      Left err -> Left err : optimizeAst stack xs inF
      Right opAst -> optimizeAst stack
          (Symbol s (Just (map fromOpti opAst)) : xs) inF
optimizeAst stack ((Call op asts) : xs) inF
  | foldr ((&&) . isUnoptimizable) True asts
      && foldr ((&&) . isValue) True asts =
      checkEval stack (Call op asts : xs) (evalAst stack (Call op asts)) inF
  | foldr ((&&) . isUnoptimizable) True asts =
      checkEvalReturnSame stack (Call op asts : xs)
      (evalAst stack (Call op asts)) inF
  | otherwise = case sequence (optimizeAst stack asts inF) of
      Left err -> Left err : optimizeAst stack xs inF
      Right asts' -> optimizeAst stack (Call op (map fromOpti asts') : xs) inF
optimizeAst stack ((Cond condAst trueAst mFalseAst) : xs) inF
  | not (isUnoptimizable condAst) = case optimizeAst stack [condAst] inF of
      [Left err] -> Left err : optimizeAst stack xs inF
      [Right (Result condAst')] ->
        optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inF
      [Right (Warning _ condAst')] ->
        optimizeAst stack (Cond condAst' trueAst mFalseAst : xs) inF
      _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inF
  | not (isUnoptimizable trueAst) = case optimizeAst stack [trueAst] inF of
      [Left err] -> Left err : optimizeAst stack xs inF
      [Right (Result trueAst')] ->
        optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inF
      [Right (Warning _ trueAst')] ->
        optimizeAst stack (Cond condAst trueAst' mFalseAst : xs) inF
      _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inF
  | isJust mFalseAst && not (isUnoptimizable (fromJust mFalseAst)) =
      case optimizeAst stack [fromJust mFalseAst] inF of
        [Left err] -> Left err : optimizeAst stack xs inF
        [Right (Result falseAst')] ->
          optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inF
        [Right (Warning _ falseAst')] ->
          optimizeAst stack (Cond condAst trueAst (Just falseAst') : xs) inF
        _ -> shouldntHappen stack (Cond condAst trueAst mFalseAst : xs) inF
  | otherwise = case condAst of
      Boolean True ->
        Right (Warning "Condition is always true" trueAst)
          : optimizeAst stack xs inF
      Boolean False ->
        Right (Warning "Condition is always false"
          (fromMaybe (Cond condAst trueAst mFalseAst) mFalseAst))
          : optimizeAst stack xs inF
      _ ->
        Right (Result (Cond condAst trueAst mFalseAst))
          : optimizeAst stack xs inF
optimizeAst stack (FunctionValue params ast Nothing : xs) inF =
  case optimizeAst stack [ast] True of
    [Left err] -> Left err : optimizeAst stack xs inF
    [Right (Result ast')] ->
      Right (Result (FunctionValue params ast' Nothing))
        : optimizeAst stack xs inF
    [Right (Warning mes ast')] ->
      Right (Warning mes (FunctionValue params ast' Nothing))
        : optimizeAst stack xs inF
    _ -> shouldntHappen stack (FunctionValue params ast Nothing : xs) inF
optimizeAst stack (FunctionValue params ast (Just asts) : xs) inF
  | not (isUnoptimizable ast) = case optimizeAst stack [ast] True of
      [Left err] -> Left err : optimizeAst stack xs inF
      [Right (Result ast')] ->
        optimizeAst stack (FunctionValue params ast' (Just asts) : xs) inF
      [Right (Warning _ ast')] ->
        optimizeAst stack (FunctionValue params ast' (Just asts) : xs) inF
      _ ->
        shouldntHappen
          stack
          (FunctionValue params ast (Just asts) : xs)
          inF
  | not (foldr ((&&) . isUnoptimizable) True asts) =
      case sequence (optimizeAst stack asts inF) of
        Left err -> Left err : optimizeAst stack xs inF
        Right asts' ->
          optimizeAst
            stack
            (FunctionValue params ast (Just (map fromOpti asts')) : xs)
            inF
  | length params > length asts =
      case evalAst stack (FunctionValue params ast (Just asts)) of
        (Left err, _) ->
          Left
            (Error err (FunctionValue params ast (Just asts)))
            : optimizeAst stack xs inF
        (Right (Just ast'), stack') ->
          Right (Result ast')
            : optimizeAst stack' xs inF
        (Right Nothing, _) ->
          shouldntHappen
            stack
            (FunctionValue params ast (Just asts) : xs)
            inF
  | otherwise =
      checkEvalReturnSame
        stack
        (FunctionValue params ast (Just asts) : xs)
        (evalAst stack (FunctionValue params ast (Just asts)))
        inF
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
isUnoptimizable (Symbol _ (Just asts)) =
  foldr ((&&) . isUnoptimizable) True asts
isUnoptimizable (FunctionValue _ ast Nothing) = isUnoptimizable ast
isUnoptimizable (FunctionValue params ast (Just asts)) =
  isUnoptimizable ast
    && foldr ((&&) . isUnoptimizable) True asts
    && length params > length asts
isUnoptimizable (Cond (Boolean _) _ _) = False
isUnoptimizable (Cond condAst bodyAst Nothing) =
  isUnoptimizable condAst && isUnoptimizable bodyAst
isUnoptimizable (Cond condAst bodyAst (Just elseAst)) =
  isUnoptimizable condAst
    && isUnoptimizable bodyAst
    && isUnoptimizable elseAst

-- | Check whether the `Ast` is a constant value
isValue :: Ast -> Bool
isValue (Value _) = True
isValue (Boolean _) = True
isValue (String _) = True
isValue (List _) = True
isValue (FunctionValue _ _ Nothing) = True
isValue _ = False

-- | Get the `Ast` contained in a `AstOptimised`
fromOpti :: AstOptimised -> Ast
fromOpti (Warning _ ast) = ast
fromOpti (Result ast) = ast

-- | Handle cases where the optimization depends on
-- the result of a evaluation of the `Ast` and it have to return evaluated
-- result
checkEval ::
  [ScopeMb] ->
  [Ast] ->
  (Either String (Maybe Ast), [ScopeMb]) ->
  Bool ->
  [Either AstError AstOptimised]
checkEval stack (ast : xs)
  (Left ('R' : 'e' : 'c' : 'u' : 'r' : 's' : 'i' : 'o' : 'n' : _), _) inF =
    Right (Warning "Possible infinite recursion" ast) :
    optimizeAst stack xs inF
checkEval stack (ast : xs)
  (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _) inF
    | inF = Right (Result ast) : optimizeAst stack xs inF
    | otherwise = Left
      (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') ast)
      : optimizeAst stack xs inF
checkEval stack (ast : xs) (Left err, _) inF =
  Left (Error err ast) : optimizeAst stack xs inF
checkEval _ (_ : xs) (Right (Just ast'), stack') inF =
  Right (Result ast') : optimizeAst stack' xs inF
checkEval stack (ast : xs) _ inF =
  shouldntHappen stack (ast : xs) inF
checkEval _ _ _ _ =
  [ Right
      ( Warning
          "This situation really shouldn't happen"
          (String "bruh")
      )
  ]

-- | Handle cases where the optimization depends on
-- the result of a evaluation of the `Ast` and it have
-- to return the original `Ast`
checkEvalReturnSame ::
  [ScopeMb] ->
  [Ast] ->
  (Either String (Maybe Ast), [ScopeMb]) ->
  Bool ->
  [Either AstError AstOptimised]
checkEvalReturnSame stack (ast : xs)
  (Left ('R' : 'e' : 'c' : 'u' : 'r' : 's' : 'i' : 'o' : 'n' : _), _) inF =
  Right (Warning "Possible infinite recursion" ast)
    : optimizeAst stack xs inF
checkEvalReturnSame stack (ast : xs)
  (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _) inF
    | inF = Right (Result ast) : optimizeAst stack xs inF
    | otherwise = Left
      (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs') ast)
      : optimizeAst stack xs inF
checkEvalReturnSame stack (ast : xs) (Left err, _) inF =
  Left (Error err ast) : optimizeAst stack xs inF
checkEvalReturnSame _ (ast : xs) (Right (Just _), stack') inF =
  Right (Result ast) : optimizeAst stack' xs inF
checkEvalReturnSame stack (ast : xs) _ inF =
  shouldntHappen stack (ast : xs) inF
checkEvalReturnSame _ _ _ _ =
  [Right (Warning "This situation really shouldn't happen" (String "bruh"))]

shouldntHappen :: [ScopeMb] -> [Ast] -> Bool -> [Either AstError AstOptimised]
shouldntHappen stack (ast : xs) inF =
  Right (Warning "This situation shouldn't happen" ast)
    : optimizeAst stack xs inF
shouldntHappen _ _ _ =
  [Right (Warning "This situation really shouldn't happen" (String "bruh"))]

checkOptiAfterDef ::
  [ScopeMb] ->
  [Either AstError AstOptimised] ->
  String ->
  Ast ->
  [Ast] ->
  Bool ->
  [Either AstError AstOptimised]
checkOptiAfterDef stack [Left err] _ _ xs inF =
  Left err : optimizeAst stack xs inF
checkOptiAfterDef stack [Right (Result opAst)] n _ xs inF =
  case evalAst stack (Define n opAst) of
    (Right _, stack') -> Right (Result (Define n opAst)) :
      optimizeAst stack' xs inF
    (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
      | inF -> Right (Result (Define n opAst)) : optimizeAst stack xs inF
      | otherwise ->
        Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs')
          (Define n opAst)) : optimizeAst stack xs inF
    (Left e, _) -> Left (Error e (Define n opAst)) : optimizeAst stack xs inF
checkOptiAfterDef stack [Right (Warning mes opAst)] n _ xs inF =
  checkEvalAfterWarningDef stack (evalAst stack (Define n opAst))
  n opAst xs inF mes
checkOptiAfterDef stack _ n ast xs inF =
  shouldntHappen stack (Define n ast : xs) inF

checkEvalAfterWarningDef ::
  [ScopeMb] ->
  (Either String (Maybe Ast), [ScopeMb]) ->
  String ->
  Ast ->
  [Ast] ->
  Bool ->
  String ->
  [Either AstError AstOptimised]
checkEvalAfterWarningDef _ (Right _, stack') n opAst xs inF mes =
  Right (Warning mes (Define n opAst)) : optimizeAst stack' xs inF
checkEvalAfterWarningDef _
  (Left ('R' : 'e' : 'c' : 'u' : 'r' : 's' : 'i' : 'o' : 'n' : _), stack')
  n opAst xs inF _ =
    Right (Warning "Possible infinite recursion" (Define n opAst))
      : optimizeAst stack' xs inF
checkEvalAfterWarningDef stack
  (Left ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs'), _)
  n opAst xs inF _
    | inF = Right (Result (Define n opAst)) : optimizeAst stack xs inF
    | otherwise =
      Left (Error ('S' : 'y' : 'm' : 'b' : 'o' : 'l' : ' ' : '\'' : xs')
        (Define n opAst)) : optimizeAst stack xs inF
checkEvalAfterWarningDef stack (Left err, _) n opAst xs inF _ =
  Left (Error err (Define n opAst)) : optimizeAst stack xs inF
