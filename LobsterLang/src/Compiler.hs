{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- Compiler
-}

module Compiler (compile, astToInstructions, compileInstructions, showInstructions, writeCompiledInstructionsToFile, Instruction(..)) where

import AST (Ast (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSUTF8
import Data.Binary
import Data.Binary.Put

-- PushArg for when user define a function
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

-- All if statement's possible forms are built-in instructions in java. do we do the same ?
data Instruction =
        -- Stack Instructions
          PushI Int
        | PushB Bool
        | PushS String -- rename to not be confused with push string
        -- Jump Instructions
        | JumpIfFalse Int
        -- Function Instructions
        | Def String Int [Instruction]
        | Call
        | Ret
        -- Built-in Functions / Operators
          -- Arithmetic Operators
          | Add
          | Sub
          | Mul
          | Div
          | Mod
          -- Comparison Operators
          | Eq
          | Less
          | LessEq
          | Great
          | GreatEq
          -- Logical Operators
          | And
          | Or
          | Not -- Used to invert if statements and Boolean values.
          -- Unary Operators
          | Neg -- Used only for negations that can not be determined at compile time (ex: Symbol negation)
        deriving (Show, Eq)

instance Enum Instruction where
  -- Stack Instructions [10 - 30]
  fromEnum (PushI _) = 10
  fromEnum (PushB _) = 11
  fromEnum (PushS _) = 12
  -- Jump Instructions [30 - 40]
  fromEnum (JumpIfFalse _) = 30
  -- Function Instructions [40 - 50]
  fromEnum (Def {}) = 40
  fromEnum Compiler.Call = 41
  fromEnum Ret = 42
  -- Built-in Functions / Operators [50 - 90]
  -- Arithmetic Operators [50 - 60]
  fromEnum Add = 50
  fromEnum Sub = 51
  fromEnum Mul = 52
  fromEnum Div = 53
  fromEnum Mod = 54
  -- Comparison Operators [60 - 70]
  fromEnum Eq = 60
  fromEnum Less = 61
  fromEnum LessEq = 62
  fromEnum Great = 63
  fromEnum GreatEq = 64
  -- Logical Operators [70 - 80]
  fromEnum And = 70
  fromEnum Or = 71
  fromEnum Not = 72
  -- Unary Operators [80 - 90]
  fromEnum Neg = 80

astToInstructions :: Ast -> [Instruction]
astToInstructions (Value value) = [PushI value]
astToInstructions (Boolean bool) = [PushB bool]
astToInstructions (Symbol symbolName) = [PushS symbolName]
astToInstructions (AST.Call "-" [Value value]) = [PushI (-value)] -- Probably useless
astToInstructions (AST.Call "-" [Symbol symbolName]) = PushS symbolName : [Neg]
astToInstructions (AST.Call "!" [Boolean bool]) = [PushB (not bool)]
astToInstructions (AST.Call "+" args) = concatMap astToInstructions args ++ [Add]
astToInstructions (AST.Call "-" args) = concatMap astToInstructions args ++ [Sub]
astToInstructions (AST.Call "*" args) = concatMap astToInstructions args ++ [Mul]
astToInstructions (AST.Call "/" args) = concatMap astToInstructions args ++ [Div]
astToInstructions (AST.Call "%" args) = concatMap astToInstructions args ++ [Mod]
astToInstructions (AST.Call "==" args) = concatMap astToInstructions args ++ [Eq]
astToInstructions (AST.Call "<" args) = concatMap astToInstructions args ++ [Less]
astToInstructions (AST.Call "<=" args) = concatMap astToInstructions args ++ [LessEq]
astToInstructions (AST.Call ">" args) = concatMap astToInstructions args ++ [Great]
astToInstructions (AST.Call ">=" args) = concatMap astToInstructions args ++ [GreatEq]
astToInstructions (AST.Call "&&" args) = concatMap astToInstructions args ++ [And]
astToInstructions (AST.Call "||" args) = concatMap astToInstructions args ++ [Or]
astToInstructions (AST.Call "!" args) = concatMap astToInstructions args ++ [Not]
astToInstructions (AST.Call funcName args) = concatMap astToInstructions args ++ [PushS funcName] ++ [Compiler.Call]
astToInstructions (Define symbolName value) = let symbolValue = astToInstructions value
                                                in [Def symbolName (length symbolValue) symbolValue]

_showInstruction :: Instruction -> Int -> String
_showInstruction (PushI value) depth = concat (replicate depth "\t") ++ "PUSH_I " ++ show value ++ "\n"
_showInstruction (PushB bool) depth = concat (replicate depth "\t") ++ "PUSH_B " ++ show bool ++ "\n"
_showInstruction (PushS symbolName) depth = concat (replicate depth "\t") ++ "PUSH_S " ++ show symbolName ++ "\n"
_showInstruction (JumpIfFalse branchOffset) depth = concat (replicate depth "\t") ++ "JUMP_IF_FALSE " ++ show branchOffset ++ "\n"
_showInstruction Add depth = concat (replicate depth "\t") ++ "ADD" ++ "\n"
_showInstruction Sub depth = concat (replicate depth "\t") ++ "SUB" ++ "\n"
_showInstruction Mul depth = concat (replicate depth "\t") ++ "MUL" ++ "\n"
_showInstruction Div depth = concat (replicate depth "\t") ++ "DIV" ++ "\n"
_showInstruction Mod depth = concat (replicate depth "\t") ++ "MOD" ++ "\n"
_showInstruction Eq depth = concat (replicate depth "\t") ++ "EQ" ++ "\n"
_showInstruction Less depth = concat (replicate depth "\t") ++ "LESS" ++ "\n"
_showInstruction LessEq depth = concat (replicate depth "\t") ++ "LESS_EQ" ++ "\n"
_showInstruction Great depth = concat (replicate depth "\t") ++ "GREAT" ++ "\n"
_showInstruction GreatEq depth = concat (replicate depth "\t") ++ "GREAT_EQ" ++ "\n"
_showInstruction And depth = concat (replicate depth "\t") ++ "AND" ++ "\n"
_showInstruction Or depth = concat (replicate depth "\t") ++ "OR" ++ "\n"
_showInstruction Not depth = concat (replicate depth "\t") ++ "NOT" ++ "\n"
_showInstruction Neg depth = concat (replicate depth "\t") ++ "NEG" ++ "\n"
_showInstruction Compiler.Call depth = concat (replicate depth "\t") ++ "CALL" ++ "\n"
_showInstruction Ret depth = concat (replicate depth "\t") ++ "RET" ++ "\n"
_showInstruction (Def symbolName nbInstruction instructions) depth
                  = concat (replicate depth "\t") ++ "DEF " ++ show symbolName ++ " <" ++ show nbInstruction ++ "> =\n" ++ _showInstructions instructions (depth + 1)

_showInstructions :: [Instruction] -> Int -> String
_showInstructions instructions depth = concatMap lambda instructions where lambda x = _showInstruction x depth

showInstructions :: [Instruction] -> IO()
showInstructions instructions = putStr (_showInstructions instructions 0)

_putOpCodeFromInstruction :: Instruction -> Put
_putOpCodeFromInstruction instruction = putWord8 (fromIntegral (fromEnum instruction))

_putString :: String -> Put
_putString string = let byteString = BSUTF8.fromString string in putInt32le (fromIntegral (BS.length byteString)) >> putByteString byteString

_putInt32 :: Int -> Put
_putInt32 value = putInt32le (fromIntegral (value::Int))

_putBool :: Bool -> Put
_putBool bool = putWord8 (fromIntegral (fromEnum bool))

_compileInstruction :: Instruction -> Put
_compileInstruction (PushI value) = _putOpCodeFromInstruction (PushI value) >> _putInt32 value
_compileInstruction (PushB bool) = _putOpCodeFromInstruction (PushB bool) >> _putBool bool
_compileInstruction (PushS symbolName) = _putOpCodeFromInstruction (PushS symbolName) >> _putString symbolName
_compileInstruction (JumpIfFalse branchOffset) = _putOpCodeFromInstruction (JumpIfFalse branchOffset) >> _putInt32 branchOffset
_compileInstruction Add = _putOpCodeFromInstruction Add
_compileInstruction Sub = _putOpCodeFromInstruction Sub
_compileInstruction Mul = _putOpCodeFromInstruction Mul
_compileInstruction Div = _putOpCodeFromInstruction Div
_compileInstruction Mod = _putOpCodeFromInstruction Mod
_compileInstruction Eq = _putOpCodeFromInstruction Eq
_compileInstruction Less = _putOpCodeFromInstruction Less
_compileInstruction LessEq = _putOpCodeFromInstruction LessEq
_compileInstruction Great = _putOpCodeFromInstruction Great
_compileInstruction GreatEq = _putOpCodeFromInstruction GreatEq
_compileInstruction And = _putOpCodeFromInstruction And
_compileInstruction Or = _putOpCodeFromInstruction Or
_compileInstruction Not = _putOpCodeFromInstruction Not
_compileInstruction Neg = _putOpCodeFromInstruction Neg
_compileInstruction Compiler.Call = _putOpCodeFromInstruction Compiler.Call
_compileInstruction Ret = _putOpCodeFromInstruction Ret
_compileInstruction (Def symbolName nbInstruction instructions)
    = _putOpCodeFromInstruction (Def symbolName nbInstruction instructions) >> _putString symbolName >> _putInt32 nbInstruction >> compileInstructions instructions

compileInstructions :: [Instruction] -> Put
compileInstructions [instruction] = _compileInstruction instruction
compileInstructions (instruction:instructions) = _compileInstruction instruction >> compileInstructions instructions

writeCompiledInstructionsToFile :: String -> Put -> IO()
writeCompiledInstructionsToFile filepath compiledInstructions = BS.writeFile filepath (BS.concat $ BSL.toChunks $ runPut compiledInstructions)

compile :: Ast -> String -> IO()
compile ast filepath = writeCompiledInstructionsToFile filepath (compileInstructions (astToInstructions ast))