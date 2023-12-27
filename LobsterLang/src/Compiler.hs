{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- Compiler
-}

module Compiler (compileAst, writeCompiledAstToFile) where

import AST (Ast (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Put
import Debug.Trace

data OpCode = CALL | PUSH | ADD
instance Enum OpCode where
  fromEnum CALL = 5
  fromEnum PUSH = 10
  fromEnum ADD = 15

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

compileArgs :: [Ast] -> Put
compileArgs [x] = compileAst x
compileArgs (x:xs) = compileAst x >> compileArgs xs

compileAst :: Ast -> Put
compileAst (Call "+" args) = compileArgs args >> trace "ADD" putOpCode ADD >> trace "CALL" putOpCode CALL
-- compileAst (Call "-" args) = "SUB " ++ compileArgs args
-- compileAst (Call "*" args) = "MUL " ++ compileArgs args
-- compileAst (Call "/" args) = "DIV " ++ compileArgs args
-- compileAst (Call "%" args) = "MOD " ++ compileArgs args
-- compileAst (Call funcName args) = "CALL " ++ funcName ++ " " ++ compileArgs args
-- compileAst (Define symbolName (Call funcName args)) = "CALLR " ++ funcName ++ " " ++ compileArgs args ++ " " ++ symbolName
-- compileAst (Define symbolName value) = putWord8 255
-- "DEF " ++ symbolName ++ " " ++ compile value
compileAst (Value value) = putOpCode PUSH >> trace ("PUSH " ++ show value) putInt32le (fromIntegral (value::Int))

writeCompiledAstToFile :: String -> Put -> IO()
writeCompiledAstToFile filepath compiledAst = BS.writeFile filepath (BS.concat $ BSL.toChunks $ runPut compiledAst)
