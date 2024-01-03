{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AstEval
-}

module AstEval
  ( sexprToAst,
    evalAst,
    evalBiValOp,
    evalBiBoolOp,
    evalBiCompValOp,
  )
where

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
-- Returns a tuple containing either the resulting Ast
-- (can be 'Nothing' for no evaluation is possible)
-- or a 'String' containing the error message in case of error
-- and the stack after evaluation.
evalAst :: [ScopeMb] -> Ast -> (Either String (Maybe Ast), [ScopeMb])
evalAst stack (Define s (FunctionValue params ast Nothing)) =
  (Right Nothing, addFuncToScope stack s params ast)
evalAst stack (Define s v) = case getVarInScope stack s of
  Nothing -> (Right Nothing, addVarToScope stack s v)
  Just _ -> (Right Nothing, updateVar stack s v)
evalAst stack (AST.Value i) = (Right (Just (AST.Value i)), stack)
evalAst stack (AST.Symbol s) =
  maybe
    (Left ("Symbol '" ++ s ++ "' doesn't exist in the current or global"), stack)
    (evalAst stack)
    (getVarInScope stack s)
evalAst stack (AST.List l) = (Right (Just (AST.List l)), stack)
evalAst stack (AST.String str) = (Right (Just (AST.String str)), stack)
evalAst stack (Boolean b) = (Right (Just (Boolean b)), stack)
evalAst stack (Call "+" astList) = evalBiValOp (+) stack (Call "+" astList)
evalAst stack (Call "-" astList) = evalBiValOp (-) stack (Call "-" astList)
evalAst stack (Call "*" astList) = evalBiValOp (*) stack (Call "*" astList)
evalAst stack (Call "/" [_, AST.Value 0]) = (Left "Cannot divide by zero", stack)
evalAst stack (Call "/" astList) = evalBiValOp div stack (Call "/" astList)
evalAst stack (Call "%" [_, AST.Value 0]) = (Left "Cannot divide by zero", stack)
evalAst stack (Call "%" astList) = evalBiValOp mod stack (Call "%" astList)
evalAst stack (Call "==" astList) = evalBiCompValOp (==) stack (Call "==" astList)
evalAst stack (Call "!=" astList) = evalBiCompValOp (/=) stack (Call "!=" astList)
evalAst stack (Call "<" astList) = evalBiCompValOp (<) stack (Call "<" astList)
evalAst stack (Call "<=" astList) = evalBiCompValOp (<=) stack (Call "<=" astList)
evalAst stack (Call ">" astList) = evalBiCompValOp (>) stack (Call ">" astList)
evalAst stack (Call ">=" astList) = evalBiCompValOp (>=) stack (Call ">=" astList)
evalAst stack (Call "&&" astList) = evalBiBoolOp (&&) stack (Call "&&" astList)
evalAst stack (Call "||" astList) = evalBiBoolOp (||) stack (Call "||" astList)
evalAst stack (Call "^^" astList) = evalBiBoolOp (\a b -> (a || b) && not (a && b)) stack (Call "||" astList)
evalAst stack (Call "!" [AST.Boolean b]) = (Right (Just (AST.Boolean (not b))), stack)
-- TODO: add ! support for evaluation of sub parameters
evalAst stack (Call "!" [_]) = (Left "Parameter of unary operator '!' isn't a boolean", stack)
evalAst stack (Call "!" _) = (Left "Invalid number of parameter for unary operator '!'", stack)
evalAst stack (Call "@" [ast]) = case astToString stack ast of
  Left err  -> (Left err, stack)
  Right ast' -> (Right (Just ast'), stack)
evalAst stack (Call "@" (_:_)) = (Left "Too much parameters for string conversion", stack)
evalAst stack (Call "@" []) = (Left "Not enough parameters for string conversion", stack)
evalAst stack (Call "++" astList) = evalBiListOp (\l el -> l ++ [el]) stack (Call "++" astList)
evalAst stack (Call "--" astList) = evalBiListOp (\l el -> filter (/= el) l) stack (Call "++" astList)
evalAst stack (Call name params) = case evalSubParams stack params of
  Left err -> (Left err, stack)
  Right asts -> case maybe (Left ("No evaluation in one or more parameters of '" ++ name ++ "'"), stack) (callFunc stack name) asts of
    (Left err', _) -> (Left err', stack)
    (Right fAst, newStack) ->
      maybe
        (Left ("Function '" ++ name ++ "' doesn't exist"), stack)
        (Data.Bifunctor.second clearScope . evalAst newStack)
        fAst
evalAst stack (FunctionValue _ _ Nothing) = (Right Nothing, stack) -- TODO: will change when function are treated as variables
evalAst stack (FunctionValue params ast (Just asts))
  | length params /= length asts =
      ( Left
          ( "Lambda takes "
              ++ show (length params)
              ++ " parameters, got "
              ++ show (length asts)
          ),
        stack
      )
  | otherwise = case evalSubParams stack asts of
      Left err -> (Left err, stack)
      Right mEAsts -> case mEAsts of
        Nothing -> (Left "No evaluation in one or more parameters of lambda", stack)
        Just eAsts -> Data.Bifunctor.second clearScope (evalAst (addVarsToScope (beginScope stack) params eAsts) ast)
evalAst stack (Cond (AST.Boolean b) a1 (Just a2))
  | b = evalAst stack a1
  | otherwise = evalAst stack a2
evalAst stack (Cond (AST.Boolean b) a1 Nothing)
  | b = evalAst stack a1
  | otherwise = (Right Nothing, stack)
evalAst stack (Cond ast a1 a2) = case fst (evalAst stack ast) of
  Left err -> (Left err, stack)
  Right mEAst -> case mEAst of
    Nothing -> (Left "No evaluation in condition", stack)
    Just eAst
      | eAst == ast -> (Left "Condition isn't a boolean", stack)
      | otherwise -> evalAst stack (Cond eAst a1 a2)

-- | Evaluate the 'Ast' for a given binary value operator
-- such as '+', '-', or '*'.
-- Takes a function that takes two 'Int' and return one 'Int',
-- the stack as a '[ScopeMb]', and the 'Ast' to evaluate.
-- Return a tuple containing the new stack post evaluation, and the
-- application of the function onto the values inside the given 'Ast'
-- or a 'String' containing the error message in case of error
evalBiValOp :: (Int -> Int -> Int) -> [ScopeMb] -> Ast -> (Either String (Maybe Ast), [ScopeMb])
evalBiValOp _ stack (Call op [AST.Boolean _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp _ stack (Call op [_, AST.Boolean _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp _ stack (Call op [AST.String _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp _ stack (Call op [_, AST.String _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp _ stack (Call op [AST.List _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp _ stack (Call op [_, AST.List _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiValOp f stack (Call _ [AST.Value a, AST.Value b]) =
  (Right (Just (AST.Value (f a b))), stack)
evalBiValOp _ stack (Call op [ast1, ast2]) = case evalSubParams stack [ast1, ast2] of
  Left err -> (Left err, stack)
  Right asts ->
    maybe
      (Left ("No evaluation in one or more parameters of binary operator '" ++ op ++ "'"), stack)
      (evalAst stack . Call op)
      asts
evalBiValOp _ stack (Call op (_ : _ : _)) = (Left ("Too much parameter for binary operator '" ++ op ++ "'"), stack)
evalBiValOp _ stack (Call op _) = (Left ("Not enough parameter for binary operator '" ++ op ++ "'"), stack)
evalBiValOp _ stack _ = (Left "Ast isn't a Call", stack)

-- | Evaluate the 'Ast' for a given binary boolean operator
-- such as '&&' or '||'.
-- Takes a function that takes two 'Bool' and return one 'Bool',
-- the stack as a '[ScopeMb]', and the 'Ast' to evaluate.
-- Return a tuple containing the new stack post evaluation, and the
-- application of the function onto the booleans inside the given 'Ast'
-- or a 'String' containing the error message in case of error
evalBiBoolOp :: (Bool -> Bool -> Bool) -> [ScopeMb] -> Ast -> (Either String (Maybe Ast), [ScopeMb])
evalBiBoolOp _ stack (Call op [AST.Value _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp _ stack (Call op [_, AST.Value _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp _ stack (Call op [AST.String _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp _ stack (Call op [_, AST.String _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp _ stack (Call op [AST.List _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp _ stack (Call op [_, AST.List _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiBoolOp f stack (Call _ [AST.Boolean a, AST.Boolean b]) =
  (Right (Just (AST.Boolean (f a b))), stack)
evalBiBoolOp _ stack (Call op [ast1, ast2]) = case evalSubParams stack [ast1, ast2] of
  Left err -> (Left err, stack)
  Right asts ->
    maybe
      (Left ("No evaluation in one or more parameters of binary operator '" ++ op ++ "'"), stack)
      (evalAst stack . Call op)
      asts
evalBiBoolOp _ stack (Call op (_ : _ : _)) = (Left ("Too much parameter for binary operator '" ++ op ++ "'"), stack)
evalBiBoolOp _ stack (Call op _) = (Left ("Not enough parameter for binary operator '" ++ op ++ "'"), stack)
evalBiBoolOp _ stack _ = (Left "Ast isn't a Call", stack)

-- | Evaluate the 'Ast' for a given binary comparison operator
-- such as '==', '>', or '<='.
-- Takes a function that takes two 'Int' and return one 'Bool',
-- the stack as a '[ScopeMb]', and the 'Ast' to evaluate.
-- Return a tuple containing the new stack post evaluation, and the
-- application of the function onto the values inside the given 'Ast'
-- or a 'String' containing the error message in case of error
evalBiCompValOp :: (Int -> Int -> Bool) -> [ScopeMb] -> Ast -> (Either String (Maybe Ast), [ScopeMb])
evalBiCompValOp _ stack (Call op [AST.Boolean _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp _ stack (Call op [_, AST.Boolean _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp _ stack (Call op [AST.String _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp _ stack (Call op [_, AST.String _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp _ stack (Call op [AST.List _, _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp _ stack (Call op [_, AST.List _]) = (Left ("One or more parameters of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiCompValOp f stack (Call _ [AST.Value a, AST.Value b]) =
  (Right (Just (AST.Boolean (f a b))), stack)
evalBiCompValOp _ stack (Call op [ast1, ast2]) = case evalSubParams stack [ast1, ast2] of
  Left err -> (Left err, stack)
  Right asts ->
    maybe
      (Left ("No evaluation in one or more parameters of binary operator '" ++ op ++ "'"), stack)
      (evalAst stack . Call op)
      asts
evalBiCompValOp _ stack (Call op (_ : _ : _)) = (Left ("Too much parameter for binary operator '" ++ op ++ "'"), stack)
evalBiCompValOp _ stack (Call op _) = (Left ("Not enough parameter for binary operator '" ++ op ++ "'"), stack)
evalBiCompValOp _ stack _ = (Left "Ast isn't a Call", stack)

-- | Evaluate the 'Ast' for a given binary list operator
-- such as '++', '--'.
-- Takes a function that takes one '[Ast]' and one 'Ast' and return one 'Ast',
-- the stack as a '[ScopeMb]', and the 'Ast' to evaluate.
-- Return a tuple containing the new stack post evaluation, and the
-- application of the function onto the values inside the given 'Ast'
-- or a 'String' containing the error message in case of error
evalBiListOp :: ([Ast] -> Ast -> [Ast]) -> [ScopeMb] -> Ast -> (Either String (Maybe Ast), [ScopeMb])
evalBiListOp _ stack (Call op [AST.Boolean _, _]) = (Left ("First parameter of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiListOp _ stack (Call op [AST.Value _, _]) = (Left ("First parameter of binary operator '" ++ op ++ "' is invalid"), stack)
evalBiListOp f stack (Call _ [AST.List a, ast]) =
  (Right (Just (AST.List (f a ast))), stack)
evalBiListOp _ stack (Call op [ast1, ast2]) = case evalSubParams stack [ast1, ast2] of
  Left err -> (Left err, stack)
  Right asts ->
    maybe
      (Left ("No evaluation in one or more parameters of binary operator '" ++ op ++ "'"), stack)
      (evalAst stack . Call op)
      asts
evalBiListOp _ stack (Call op (_ : _ : _)) = (Left ("Too much parameter for binary operator '" ++ op ++ "'"), stack)
evalBiListOp _ stack (Call op _) = (Left ("Not enough parameter for binary operator '" ++ op ++ "'"), stack)
evalBiListOp _ stack _ = (Left "Ast isn't a Call", stack)

-- | Evaluate the list of 'Ast'
-- Takes the stack as a '[ScopeMb]' and a '[Ast]' to evaluate
-- Returns a list of the results of the evaluation
-- (can be 'Nothing' if one 'Ast' isn't evaluable)
-- or a 'String' containing the error message in case of error.
evalSubParams :: [ScopeMb] -> [Ast] -> Either String (Maybe [Ast])
evalSubParams stack astList = case mapM (fst . evalAst stack) astList of
  Left err -> Left err
  Right asts -> Right (sequence asts)

astToString :: [ScopeMb] -> Ast -> Either String Ast
astToString _ (AST.String str) = Right (AST.String str)
astToString _ (AST.Value val) = Right (AST.String (show val))
astToString _ (AST.Boolean bool) = Right (AST.String (show bool))
astToString _ (AST.FunctionValue _ _ Nothing) = Left "Cannot convert lambda to string"
astToString stack ast = case evalAst stack ast of
  (Left err, _) -> Left err
  (Right ast', _) -> maybe (Left "Cannot convert no evaluation to string")
    (astToString stack) ast'
