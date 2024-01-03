{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Environment
-}

module Scope
  ( ScopeMb (..),
    beginScope,
    clearScope,
    addVarToScope,
    getVarInScope,
    addVarsToScope,
    addFuncToScope,
    callFunc,
  )
where

import AST
import Data.Maybe
import Stack

-- | Structure representing a member in a scope.
-- Can be a 'ScopeBegin' to mark the beginning of a scope or
-- a 'Variable' containing its name as a 'String' and its value as an 'Ast'
data ScopeMb
  = ScopeBegin
  | Variable String Ast
  | Function String [String] Ast
  deriving (Eq, Show)

-- | Begin a new scope by adding a 'ScopeBegin' to the stack.
beginScope :: [ScopeMb] -> [ScopeMb]
beginScope s = push s ScopeBegin

-- | Clear the current scope
clearScope :: [ScopeMb] -> [ScopeMb]
clearScope (x : xs)
  | maybe True (ScopeBegin ==) (fst result) = snd result
  | otherwise = clearScope xs
  where
    result = pop (x : xs)
clearScope [] = []

-- | add a Function to the current scope with its name as a 'String',
-- a list of its parameters as 'String' and the abstarct tree of the function
addFuncToScope :: [ScopeMb] -> String -> [String] -> Ast -> [ScopeMb]
addFuncToScope stack name params ast = push stack (Function name params ast)

-- | Call a function from the current scope
callFunc :: [ScopeMb] -> String -> [Ast] -> (Either String (Maybe Ast), [ScopeMb])
callFunc stack name asts
  | isNothing func = (Left ("Function '" ++ name ++ "' not found"), stack)
  | null newStack = (Left ("Function '" ++ name ++ "' takes " ++
    show (length (getFuncParamNames (fromJust func))) ++
    " parameters, got " ++ show (length asts)), stack)
  | otherwise = (Right (getAst (fromJust func)), newStack)
  where
    func = seek (isSearchedFunc name) stack
    newStack =
      createFuncVar
        (beginScope stack)
        (maybe [] getFuncParamNames func)
        asts

createFuncVar :: [ScopeMb] -> [String] -> [Ast] -> [ScopeMb]
createFuncVar stack [] [] = stack
createFuncVar _ [] _ = []
createFuncVar _ _ [] = []
createFuncVar stack (name : nxs) (ast : axs) =
  createFuncVar
    (addVarToScope stack name ast)
    nxs
    axs

-- | Add a Variable to the stack with its name as a 'String'
-- and its value by an 'Ast'
addVarToScope :: [ScopeMb] -> String -> Ast -> [ScopeMb]
addVarToScope stack s v = push stack (Variable s v)

-- | Add multiple variables to the stack with their names as a 'String'
-- and their values as an 'Ast'
addVarsToScope :: [ScopeMb] -> [String] -> [Ast] -> [ScopeMb]
addVarsToScope stack [] _ = stack
addVarsToScope stack _ [] = stack
addVarsToScope stack (s : xs1) (v : xs2) =
  push
    (addVarsToScope stack xs1 xs2)
    (Variable s v)

-- | Get the value contained in the variable given by name as a 'String',
-- return 'Nothing' if the variable don't exist or 'Just' its value
getVarInScope :: [ScopeMb] -> String -> Maybe Ast
getVarInScope stack s = getAst =<< seek (isSearchedVar s) stack

-- | Get the 'Ast' at a given 'ScopeMb'
getAst :: ScopeMb -> Maybe Ast
getAst (Variable _ ast) = Just ast
getAst (Function _ _ ast) = Just ast
getAst _ = Nothing

getFuncParamNames :: ScopeMb -> [String]
getFuncParamNames (Function _ names _) = names
getFuncParamNames _ = []

-- | Return 'True' if the 'ScopeMb' is a Function
-- by the name passed as parameter
isSearchedFunc :: String -> ScopeMb -> Bool
isSearchedFunc s2 (Function s1 _ _)
  | s1 == s2 = True
  | otherwise = False
isSearchedFunc _ _ = False

-- | Return 'True' if the 'ScopeMb' is a Variable
-- by the name passed as parameter
isSearchedVar :: String -> ScopeMb -> Bool
isSearchedVar s2 (Variable s1 _)
  | s1 == s2 = True
  | otherwise = False
isSearchedVar _ _ = False
