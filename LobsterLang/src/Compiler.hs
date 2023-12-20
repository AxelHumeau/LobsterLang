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

compileArg :: Ast -> String
compileArg (Value value) = show value
compileArg (Symbol symbol) = symbol
compileArg (Boolean bool) = show bool

compileArgs :: [Ast] -> String
compileArgs [] = ""
compileArgs [x] = compileArg x
compileArgs (x:xs) = compileArg x ++ " " ++ compileArgs xs

compileAst :: Ast -> String
compileAst (Value v) = "VAL" ++ " " ++ show v
compileAst (Call funcName args) = "CALL " ++ funcName ++ " " ++ compileArgs args
