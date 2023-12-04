{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Environment
-}

module Scope(ScopeMb(..),
             beginScope,
             clearScope,
             addVarToScope,
             getVarInScope) where

import Stack
import AST

-- | Structure representing a member in a scope.
-- Can be a 'ScopeBegin' to mark the beginning of a scope or
-- a 'Variable' containing its name as a 'string' and its value as an 'Ast'
data ScopeMb = ScopeBegin |
                Variable String Ast
                deriving (Eq, Show)

-- | Begin a new scope by adding a 'ScopeBegin' to the stack.
beginScope :: [ScopeMb] -> [ScopeMb]
beginScope s = push s ScopeBegin

-- | Clear the current scope
clearScope :: [ScopeMb] -> [ScopeMb]
clearScope (x:xs) | maybe True (ScopeBegin ==) (fst result) = (snd result)
                  | otherwise = clearScope xs
                  where result = pop (x:xs)
clearScope [] = []

-- | Add a Variable to the stack with its name as a 'string'
-- and its value by an 'Ast'
addVarToScope :: [ScopeMb] -> String -> Ast -> [ScopeMb]
addVarToScope stack s v = (Variable s v:stack)

-- | Get the value contained in the variable given by name as a 'string',
-- return 'Nothing' if the variable don't exist or 'Just' its value
getVarInScope :: [ScopeMb] -> String -> Maybe Ast
getVarInScope stack s = maybe Nothing getVarValue
    (seek (isSearchedVar s) stack)

-- | Get the Value at a given 'ScopeMb'
getVarValue :: ScopeMb -> Maybe Ast
getVarValue (Variable _ v) = Just v
getVarValue _ = Nothing

-- | Return 'True' if the 'ScopeMb' is a Variable
-- by the name passed as parameter
isSearchedVar :: String -> ScopeMb -> Bool
isSearchedVar s2 (Variable s1 _) | s1 == s2 = True
                                 | otherwise = False
isSearchedVar _ _ = False
