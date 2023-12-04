{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AST
-}

module AST (Ast(..)) where

data Ast = Define String Ast
        | Integer Int
        | Symbol String
        | Boolean Bool
        | Call String [Ast]
        deriving (Show, Eq)

