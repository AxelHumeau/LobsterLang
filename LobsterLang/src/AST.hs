{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AST
-}

module AST (Ast(..)) where

-- | Abstract syntax tree for representing instructions
data Ast = Define String Ast
        | Value Int
        | Symbol String
        | Boolean Bool
        | Call String [Ast]
        | FunctionValue [String] Ast (Maybe [Ast])
        deriving (Show, Eq)
