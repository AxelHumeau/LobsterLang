{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Environment
-}

module Interpreter(Interpreter(..), ScopeMem(..)) where

import AST

data ScopeMem = ScopeBegin |
                VariableInt String Int |
                VariableBool String Bool

data Interpreter = Interpreter {
    tree :: Ast,
    scopeStack :: [ScopeMem],
    globalStack :: [ScopeMem]
}

beginScope :: [ScopeMem] -> [ScopeMem]
beginScope s = (ScopeBegin:s)

