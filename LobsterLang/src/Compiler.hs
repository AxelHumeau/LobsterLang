{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- Compiler
-}

module Compiler (compileAst, writeCompiledAstToFile) where

import AST (Ast (..))
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BLU
import Data.ByteString.Char8 as C8
-- import Data.Binary.Put

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put

data OpCode = PUSH
instance Enum OpCode where
  fromEnum PUSH = 10

putOpCode :: OpCode -> Put
putOpCode opCode = putWord8 (fromIntegral (fromEnum opCode))

-- absCode =
    -- PushArg 0
    -- Push 0
    -- Push Less
    -- Call
    -- JumpIfFalse 2
    -- PushArg 0
    -- Ret
    -- PushArg 0
    -- Push -1
    -- Push Mul
    -- Call
    -- Ret
-- Push -42
-- Push absCode
-- Call
-- Ret

compileArg :: Ast -> String
compileArg (Value value) = show value
compileArg (Symbol symbol) = symbol
compileArg (Boolean bool) = show bool

compileArgs :: [Ast] -> String
compileArgs [] = ""
compileArgs [x] = compileArg x
compileArgs (x:xs) = compileArg x ++ " " ++ compileArgs xs

compileAst :: Ast -> Put
-- compileAst (Call "+" args) = putWord "ADD " ++ compileArgs args
-- compileAst (Call "-" args) = "SUB " ++ compileArgs args
-- compileAst (Call "*" args) = "MUL " ++ compileArgs args
-- compileAst (Call "/" args) = "DIV " ++ compileArgs args
-- compileAst (Call "%" args) = "MOD " ++ compileArgs args
-- compileAst (Call funcName args) = "CALL " ++ funcName ++ " " ++ compileArgs args
-- compileAst (Define symbolName (Call funcName args)) = "CALLR " ++ funcName ++ " " ++ compileArgs args ++ " " ++ symbolName
-- compileAst (Define symbolName value) = putWord8 255
-- "DEF " ++ symbolName ++ " " ++ compileArg value
compileAst (Value value) = putOpCode PUSH >> putInt32le (fromIntegral (value::Int)) -- push/10/0a arg0::i32 # push a 32bit int value on the stack

writeCompiledAstToFile :: String -> Put -> IO()
-- writeCompiledAstToFile filepath compiledAst = B.writeFile filepath (C8.pack compiledAst)
writeCompiledAstToFile filepath compiledAst = B.writeFile filepath (B.concat $ BL.toChunks $ runPut compiledAst)
