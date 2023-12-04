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

data ScopeMb = ScopeBegin |
                Variable String Ast
                deriving (Eq, Show)

beginScope :: [ScopeMb] -> [ScopeMb]
beginScope s = (ScopeBegin:s)

clearScope :: [ScopeMb] -> [ScopeMb]
clearScope (x:xs) | maybe True (ScopeBegin ==) (fst result) = (snd result)
                  | otherwise = clearScope xs
                  where result = pop (x:xs)
clearScope [] = []

addVarToScope :: [ScopeMb] -> String -> Ast -> [ScopeMb]
addVarToScope stack s v = (Variable s v:stack)

getVarInScope :: [ScopeMb] -> String -> Maybe Ast
getVarInScope stack s = maybe Nothing getVarValue
    (seek (isSearchedVar s) stack)

getVarValue :: ScopeMb -> Maybe Ast
getVarValue (Variable _ v) = Just v
getVarValue _ = Nothing

isSearchedVar :: String -> ScopeMb -> Bool
isSearchedVar s2 (Variable s1 _) | s1 == s2 = True
                                 | otherwise = False
isSearchedVar _ _ = False
