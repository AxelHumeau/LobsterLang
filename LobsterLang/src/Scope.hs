{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Environment
-}

module Scope(ScopeMb(..), beginScope, clearScope) where

import Stack

data ScopeMb = ScopeBegin |
                Variable String Int
                deriving Eq

beginScope :: [ScopeMb] -> [ScopeMb]
beginScope s = (ScopeBegin:s)

clearScope :: [ScopeMb] -> [ScopeMb]
clearScope (x:xs) | maybe True (ScopeBegin ==) (fst result) = (snd result)
                  | otherwise = clearScope xs
                  where result = pop (x:xs)
clearScope [] = []

