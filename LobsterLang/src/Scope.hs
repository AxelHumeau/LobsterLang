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
    updateVar,
  )
where

import AST
import Data.Maybe
import Stack

-- | Structure representing a member in a scope.
-- Can be a 'ScopeBegin' to mark the beginning of a scope or
-- a 'Variable' containing its name as a 'String' and its value as an 'Ast'
data ScopeMb
  = ScopeBegin Int
  | Variable String Ast Int
  | Function String [String] Ast Int
  deriving (Eq, Show)

-- | Get the depth of a member in a scope
getDepth :: [ScopeMb] -> Int
getDepth s = case top s of
  Just (ScopeBegin depth) -> depth
  Just (Variable _ _ depth) -> depth
  Just (Function _ _ _ depth) -> depth
  Nothing -> -1

-- | Begin a new scope by adding a 'ScopeBegin' to the stack.
beginScope :: [ScopeMb] -> [ScopeMb]
beginScope s = push s (ScopeBegin (getDepth s))

-- | Clear the current scope
clearScope :: [ScopeMb] -> [ScopeMb]
clearScope [] = []
clearScope s = case pop s of
  (Nothing, s') -> s'
  (Just (ScopeBegin _), s') -> s'
  (Just _, s') -> clearScope s'

-- | add a Function to the current scope with its name as a 'String',
-- a list of its parameters as 'String' and the abstarct tree of the function
addFuncToScope :: [ScopeMb] -> String -> [String] -> Ast -> [ScopeMb]
addFuncToScope stack name params ast = push stack (Function name params ast (getDepth stack + 1))

-- | Call a function from the current scope
callFunc :: [ScopeMb] -> String -> [Ast] -> (Either String (Maybe Ast), [ScopeMb])
callFunc stack name asts
  | isNothing func = (Left ("Function '" ++ name ++ "' not found"), stack)
  | null newStack = (Left ("Function '" ++ name ++ "' takes " ++
    show (length (getFuncParamNames (fromJust func))) ++
    " parameters, got " ++ show (length asts)), stack)
  | otherwise = (Right (getAst (fromJust func)), newStack)
  where
    func = seek (isSearchedFunc name (getDepth stack)) stack
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
addVarToScope stack s v = push stack (Variable s v (getDepth stack + 1))

-- | Add multiple variables to the stack with their names as a 'String'
-- and their values as an 'Ast'
addVarsToScope :: [ScopeMb] -> [String] -> [Ast] -> [ScopeMb]
addVarsToScope stack [] _ = stack
addVarsToScope stack _ [] = stack
addVarsToScope stack (s : xs1) (v : xs2) =
  push
    (addVarsToScope stack xs1 xs2)
    (Variable s v (getDepth stack))

seekAndUpdate :: [ScopeMb] -> Int -> String -> Ast -> [ScopeMb]
seekAndUpdate (h:xs) depth name ast
  | isSearchedVar name depth h = Variable name ast depth : xs
  | otherwise = h : seekAndUpdate xs depth name ast
seekAndUpdate [] _ _ _ = []

-- | Update the given variable, don't do anything if it doesn't exist
updateVar :: [ScopeMb] -> String -> Ast -> [ScopeMb]
updateVar stack = seekAndUpdate stack (getDepth stack)

-- | Get the value contained in the variable given by name as a 'String',
-- return 'Nothing' if the variable don't exist or 'Just' its value
getVarInScope :: [ScopeMb] -> String -> Maybe Ast
getVarInScope stack s = getAst =<< seek (isSearchedVar s (getDepth stack)) stack

-- | Get the 'Ast' at a given 'ScopeMb'
getAst :: ScopeMb -> Maybe Ast
getAst (Variable _ ast _) = Just ast
getAst (Function _ _ ast _) = Just ast
getAst _ = Nothing

getFuncParamNames :: ScopeMb -> [String]
getFuncParamNames (Function _ names _ _) = names
getFuncParamNames _ = []

-- | Return 'True' if the 'ScopeMb' is a Function
-- by the name passed as parameter and if the function is in the current scope
-- or global scope
isSearchedFunc :: String -> Int -> ScopeMb -> Bool
isSearchedFunc s2 currentDepth (Function s1 _ _ depth) =
  s1 == s2 && (depth == 0 || depth == currentDepth)
isSearchedFunc _ _ _ = False

-- | Return 'True' if the 'ScopeMb' is a Variable
-- by the name passed as parameter and if the variable is in the current scope
-- or global scope
isSearchedVar :: String -> Int -> ScopeMb -> Bool
isSearchedVar s2 currentDepth (Variable s1 _ depth) =
  s1 == s2 && (depth == 0 || depth == currentDepth)
isSearchedVar _ _ _ = False
