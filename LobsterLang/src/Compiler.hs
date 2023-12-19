{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- Compiler
-}

module Compiler (compileAst) where

import AST (Ast (..))
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.Char8 as BLU
-- import Data.Binary.Put

compileAst :: Ast -> String
compileAst (Value v) = "VAL" ++ " " ++ show v
