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
        | Boolean Bool
        | String String
        | List [Ast]
        | Symbol String (Maybe [Ast])
        | Call String [Ast]
        | FunctionValue [String] Ast (Maybe [Ast])
        | Cond Ast Ast (Maybe Ast)
        deriving (Show, Eq)
