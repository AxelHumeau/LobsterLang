{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- Compiler
-}

-- BONUS TODO: basic java trans compilation

module Compiler (
  compile,
  astToInstructions,
  compileInstructions,
  showInstructions,
  writeCompiledInstructionsToFile,
  Instruction(..)
) where

import AST (Ast (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSUTF8
import Data.Binary
import Data.Binary.Put
import qualified Data.List

data CompileConstants = Null
  | MagicNumber deriving (Show, Eq)
instance Enum CompileConstants where
  fromEnum MagicNumber = 763
  fromEnum Null = 0
  toEnum 763 = MagicNumber
  toEnum _ = Null

data Instruction =
    NoOp
  -- Stack Instructions
  | PushI Int
  | PushB Bool
  | PushSym String (Maybe [[Instruction]])
  | PushStr String
  | PushList Int [[Instruction]]
  | PushArg Int
  -- Jump Instructions
  | Jump Int
  | JumpIfFalse Int
  -- Function Instructions
  | Def String Int [Instruction]
  | Fnv Int [String] Int [Instruction] [Int] (Maybe [[Instruction]])
  | Call -- ()
  | Ret
  -- Logical Instructions
  | Cond [Instruction] Int [Instruction] (Maybe [Instruction])
  -- Built-in Functions / Operators
    -- Arithmetic Operators
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | XorB -- ^^
    -- Comparison Operators
    | Eq
    | NotEq
    | Less
    | LessEq
    | Great
    | GreatEq
    -- Logical Operators
    | And
    | Or
    | Not -- Used to invert if statements and Boolean values.
    | Then
    -- Unary Operators
    | ToStr -- @
    | Neg -- Used only for negations that can not be determined at compile time (ex: Symbol negation)
    -- Built-in Functions
    | Apnd -- ++
    | RemAllOcc -- --
    | Get -- !!
    | Len -- len
  deriving (Show, Eq)

instance Enum Instruction where
  fromEnum NoOp = 0
  -- Stack Instructions [10 - 30]
  fromEnum (PushI _) = 10
  fromEnum (PushB _) = 11
  fromEnum (PushSym _ _) = 12
  fromEnum (PushStr _) = 13
  fromEnum (PushList _ _) = 14
  fromEnum (PushArg _) = 15
  -- Jump Instructions [30 - 40]
  fromEnum (Jump _) = 30
  fromEnum (JumpIfFalse _) = 31
  -- Function Instructions [40 - 45]
  fromEnum (Def {}) = 40
  fromEnum (Fnv {}) = 41
  fromEnum Compiler.Call = 42
  fromEnum Ret = 43
  -- Logical Instructions [45 - 50]
  fromEnum (Compiler.Cond {}) = 45
  -- Built-in Functions / Operators [50 - 90]
  -- Arithmetic Operators [50 - 60]
  fromEnum Add = 50
  fromEnum Sub = 51
  fromEnum Mul = 52
  fromEnum Div = 53
  fromEnum Mod = 54
  fromEnum XorB = 55
  -- Comparison Operators [60 - 70]
  fromEnum Eq = 60
  fromEnum NotEq = 61
  fromEnum Less = 62
  fromEnum LessEq = 63
  fromEnum Great = 64
  fromEnum GreatEq = 65
  -- Logical Operators [70 - 80]
  fromEnum And = 70
  fromEnum Or = 71
  fromEnum Not = 72
  fromEnum Then = 73
  -- Unary Operators [80 - 90]
  fromEnum ToStr = 80
  fromEnum Neg = 81
  -- Built-in Functions [100 - ...]
  fromEnum Apnd = 100
  fromEnum RemAllOcc = 101
  fromEnum Get = 102
  fromEnum Len = 103

  toEnum 0 = NoOp
  toEnum 10 = PushI 0
  toEnum 11 = PushB False
  toEnum 12 = PushSym "" Nothing
  toEnum 13 = PushStr ""
  toEnum 14 = PushList 0 []
  toEnum 15 = PushArg 0
  toEnum 30 = Jump 0
  toEnum 31 = JumpIfFalse 0
  toEnum 40 = Def "" 0 []
  toEnum 41 = Fnv 0 [] 0 [] [] Nothing
  toEnum 42 = Compiler.Call
  toEnum 43 = Ret
  toEnum 45 = Compiler.Cond [] 0 [] Nothing
  toEnum 50 = Add
  toEnum 51 = Sub
  toEnum 52 = Mul
  toEnum 53 = Div
  toEnum 54 = Mod
  toEnum 55 = XorB
  toEnum 60 = Eq
  toEnum 61 = NotEq
  toEnum 62 = Less
  toEnum 63 = LessEq
  toEnum 64 = Great
  toEnum 65 = GreatEq
  toEnum 70 = And
  toEnum 71 = Or
  toEnum 72 = Not
  toEnum 73 = Then
  toEnum 80 = ToStr
  toEnum 81 = Neg
  toEnum 100 = Apnd
  toEnum 101 = RemAllOcc
  toEnum 102 = Get
  toEnum 103 = Len
  toEnum _ = NoOp

astToInstructions :: Ast -> [Instruction]
astToInstructions (Value value) = [PushI value]
astToInstructions (Boolean bool) = [PushB bool]
astToInstructions (Symbol symbolName Nothing) = [PushSym symbolName Nothing]
astToInstructions (Symbol symbolName (Just symbolArgs)) =
  [PushSym symbolName (Just symbolArgsInstructions)]
  where
    symbolArgsInstructions = map astToInstructions symbolArgs
astToInstructions (String stringValue) = [PushStr stringValue]
astToInstructions (List values) =
  [PushList (length valuesInstructions) valuesInstructions]
  where
    valuesInstructions = map astToInstructions values
astToInstructions (AST.Call "+" args) =
  concatMap astToInstructions args ++ [Add]
astToInstructions (AST.Call "-" args) =
  concatMap astToInstructions args ++ [Sub]
astToInstructions (AST.Call "*" args) =
  concatMap astToInstructions args ++ [Mul]
astToInstructions (AST.Call "/" args) =
  concatMap astToInstructions args ++ [Div]
astToInstructions (AST.Call "%" args) =
  concatMap astToInstructions args ++ [Mod]
astToInstructions (AST.Call "^^" args) =
  concatMap astToInstructions args ++ [XorB]
astToInstructions (AST.Call "==" args) =
  concatMap astToInstructions args ++ [Eq]
astToInstructions (AST.Call "!=" args) =
  concatMap astToInstructions args ++ [NotEq]
astToInstructions (AST.Call "<" args) =
  concatMap astToInstructions args ++ [Less]
astToInstructions (AST.Call "<=" args) =
  concatMap astToInstructions args ++ [LessEq]
astToInstructions (AST.Call ">" args) =
  concatMap astToInstructions args ++ [Great]
astToInstructions (AST.Call ">=" args) =
  concatMap astToInstructions args ++ [GreatEq]
astToInstructions (AST.Call "&&" args) =
  concatMap astToInstructions args ++ [And]
astToInstructions (AST.Call "||" args) =
  concatMap astToInstructions args ++ [Or]
astToInstructions (AST.Call "!" args) =
  concatMap astToInstructions args ++ [Not]
astToInstructions (AST.Call "$" args) =
  concatMap astToInstructions args ++ [Then]
astToInstructions (AST.Call "@" args) =
  concatMap astToInstructions args ++ [ToStr]
astToInstructions (AST.Call "++" args) =
  concatMap astToInstructions args ++ [Apnd]
astToInstructions (AST.Call "--" args) =
  concatMap astToInstructions args ++ [RemAllOcc]
astToInstructions (AST.Call "!!" args) =
  concatMap astToInstructions args ++ [Get]
astToInstructions (AST.Call "len" args) =
  concatMap astToInstructions args ++ [Len]
astToInstructions (AST.Call _ _) = [NoOp]
astToInstructions (Define symbolName value) =
  let symbolValue = astToInstructions value
  in [Def symbolName (length symbolValue) symbolValue]
astToInstructions (FunctionValue argsNames funcBody Nothing) =
  [ Fnv
    (length argsNames)
    argsNames
    (length funcBodyInstructions)
    funcBodyInstructions
    []
    Nothing ]
  where
    funcBodyInstructions =
      _resolveFunctionPushArgs (astToInstructions funcBody ++ [Ret]) argsNames
astToInstructions (FunctionValue argsNames funcBody (Just argsValues)) =
  [ Fnv
    (length argsNames)
    argsNames
    (length funcBodyInstructions)
    funcBodyInstructions
    nbArgsValuesInstructions
    argsValuesInstructions ]
  where
    funcBodyInstructions =
      _resolveFunctionPushArgs (astToInstructions funcBody ++ [Ret]) argsNames
    argsValuesInstructions = Just (map astToInstructions argsValues)
    nbArgsValuesInstructions = _instructionListLengths argsValuesInstructions
astToInstructions (AST.Cond cond trueBlock (Just falseBlock)) =
  [ Compiler.Cond
    condInstructions
    nbTrueBlockInstructions
    trueBlockInstructions
    (Just falseBlockInstructions) ]
  where
    condInstructions = astToInstructions cond
    falseBlockInstructions = astToInstructions falseBlock
    trueBlockInstructions =
      astToInstructions trueBlock ++ [Jump (length falseBlockInstructions)]
    nbTrueBlockInstructions = length trueBlockInstructions
astToInstructions (AST.Cond cond trueBlock Nothing) =
  [ Compiler.Cond
    condInstructions
    nbTrueBlockInstructions
    trueBlockInstructions
    Nothing ]
  where
    condInstructions = astToInstructions cond
    trueBlockInstructions =
      astToInstructions trueBlock
    nbTrueBlockInstructions = length trueBlockInstructions

_showInstruction :: Instruction -> Int -> [Char]
_showInstruction NoOp _ = "NO_OP\n"
_showInstruction (PushI value) depth =
  concat (replicate depth "\t") ++ "PUSH_I " ++ show value ++ "\n"
_showInstruction (PushB bool) depth =
  concat (replicate depth "\t") ++ "PUSH_B " ++ show bool ++ "\n"
_showInstruction (PushStr stringValue) depth =
  concat (replicate depth "\t") ++ "PUSH_STR " ++ stringValue ++ "\n"
_showInstruction (PushSym symbolName Nothing) depth =
    concat (replicate depth "\t") ++
    "PUSH_SYM " ++
    symbolName ++ "\n"
_showInstruction (PushSym symbolName (Just symbolArgs)) depth =
    concat (replicate depth "\t") ++
    "PUSH_SYM " ++
    symbolName ++ " " ++
    "(" ++ show (length symbolArgs) ++ ")" ++
    "[\n" ++ _showInstructionList symbolArgs (depth + 1) ++ "]\n"
_showInstruction (PushList nbValuesInstructions valuesInstructions) depth =
    concat (replicate depth "\t") ++
    "PUSH_LIST " ++
    "(" ++ show nbValuesInstructions ++ ")" ++
    "[\n" ++ _showInstructionList valuesInstructions (depth + 1) ++ "]\n"
_showInstruction (PushArg index) depth =
  concat (replicate depth "\t") ++ "PUSH_ARG " ++ show index ++ "\n"
_showInstruction (Jump branchOffset) depth =
  concat (replicate depth "\t")
  ++ "JUMP "
  ++ show branchOffset ++ "\n"
_showInstruction (JumpIfFalse branchOffset) depth =
  concat (replicate depth "\t")
  ++ "JUMP_IF_FALSE "
  ++ show branchOffset ++ "\n"
_showInstruction Add depth =
  concat (replicate depth "\t") ++ "ADD" ++ "\n"
_showInstruction Sub depth =
  concat (replicate depth "\t") ++ "SUB" ++ "\n"
_showInstruction Mul depth =
  concat (replicate depth "\t") ++ "MUL" ++ "\n"
_showInstruction Div depth =
  concat (replicate depth "\t") ++ "DIV" ++ "\n"
_showInstruction Mod depth =
  concat (replicate depth "\t") ++ "MOD" ++ "\n"
_showInstruction XorB depth =
  concat (replicate depth "\t") ++ "XOR_B" ++ "\n"
_showInstruction Eq depth =
  concat (replicate depth "\t") ++ "EQ" ++ "\n"
_showInstruction NotEq depth =
  concat (replicate depth "\t") ++ "NOT_EQ" ++ "\n"
_showInstruction Less depth =
  concat (replicate depth "\t") ++ "LESS" ++ "\n"
_showInstruction LessEq depth =
  concat (replicate depth "\t") ++ "LESS_EQ" ++ "\n"
_showInstruction Great depth =
  concat (replicate depth "\t") ++ "GREAT" ++ "\n"
_showInstruction GreatEq depth =
  concat (replicate depth "\t") ++ "GREAT_EQ" ++ "\n"
_showInstruction And depth =
  concat (replicate depth "\t") ++ "AND" ++ "\n"
_showInstruction Or depth =
  concat (replicate depth "\t") ++ "OR" ++ "\n"
_showInstruction Not depth =
  concat (replicate depth "\t") ++ "NOT" ++ "\n"
_showInstruction Then depth =
  concat (replicate depth "\t") ++ "THEN" ++ "\n"
_showInstruction ToStr depth =
  concat (replicate depth "\t") ++ "TO_STR" ++ "\n"
_showInstruction Neg depth =
  concat (replicate depth "\t") ++ "NEG" ++ "\n"
_showInstruction Compiler.Call depth =
  concat (replicate depth "\t") ++ "CALL" ++ "\n"
_showInstruction Ret depth = concat (replicate depth "\t") ++ "RET" ++ "\n"
_showInstruction (Def symbolName nbInstruction instructions) depth =
  concat (replicate depth "\t") ++ "DEF " ++ show symbolName ++ " (" ++
  show nbInstruction ++ ") =\n" ++ _showInstructions instructions (depth + 1)
_showInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
  funcBodyInstructions nbArgsValuesInstructions
  (Just argsValuesInstructions)) depth =
    concat (replicate depth "\t") ++
    "FNV " ++
    "(" ++ show nbArgsNames ++ ")" ++
    show argsNames ++
    " (" ++ show nbArgsValuesInstructions ++ ")" ++
    "(\n" ++ _showInstructionList argsValuesInstructions (depth + 1) ++ ")" ++
    " = (" ++ show nbFuncBodyInstructions ++
    "){\n" ++ _showInstructions funcBodyInstructions (depth + 1) ++ "}\n"

_showInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
  funcBodyInstructions _ Nothing) depth =
    concat (replicate depth "\t") ++
    "FNV " ++
    "(" ++ show nbArgsNames ++ ")" ++
    show argsNames ++
    " = (" ++ show nbFuncBodyInstructions ++
    "){\n" ++ _showInstructions funcBodyInstructions (depth + 1) ++ "}\n"
_showInstruction (Compiler.Cond condInstructions
  nbTrueBlockInstructions trueBlockInstructions
  (Just falseBlockInstructions)) depth =
    concat (replicate depth "\t") ++
    "COND " ++
    "(" ++ show (length condInstructions) ++ ")" ++
    "(\n" ++ _showInstructions condInstructions (depth + 1) ++
    _showInstruction (JumpIfFalse nbTrueBlockInstructions) 0 ++ ")" ++
    " true: (" ++ show nbTrueBlockInstructions ++
    "){\n" ++ _showInstructions trueBlockInstructions (depth + 1) ++ "}" ++
    " false: (" ++ show (length falseBlockInstructions) ++
    "){\n" ++ _showInstructions falseBlockInstructions (depth + 1) ++ "}\n"
_showInstruction (Compiler.Cond condInstructions
  nbTrueBlockInstructions trueBlockInstructions Nothing) depth =
    concat (replicate depth "\t") ++
    "COND " ++
    "(" ++ show (length condInstructions) ++ ")" ++
    "(\n" ++ _showInstructions condInstructions (depth + 1) ++
    _showInstruction (JumpIfFalse nbTrueBlockInstructions) 0 ++ ")" ++
    " true: (" ++ show nbTrueBlockInstructions ++
    "){\n" ++ _showInstructions trueBlockInstructions (depth + 1) ++ "}" ++
    " false: {}\n"
_showInstruction Apnd depth =
  concat (replicate depth "\t") ++ "APND" ++ "\n"
_showInstruction RemAllOcc depth =
  concat (replicate depth "\t") ++ "REM_ALL_OCC" ++ "\n"
_showInstruction Get depth =
  concat (replicate depth "\t") ++ "GET" ++ "\n"
_showInstruction Len depth =
  concat (replicate depth "\t") ++ "LEN" ++ "\n"

_resolveFunctionPushArgs :: [Instruction] -> [String] -> [Instruction]
_resolveFunctionPushArgs [] _ = []
_resolveFunctionPushArgs [PushSym symbolName args] argsNames =
  case Data.List.elemIndex symbolName argsNames of
    Just value -> [PushArg value]
    Nothing -> [PushSym symbolName args]
_resolveFunctionPushArgs [instruction] _ = [instruction]
_resolveFunctionPushArgs (instruction:instructions) argsNames
  = _resolveFunctionPushArgs [instruction] argsNames
  ++ _resolveFunctionPushArgs instructions argsNames

_instructionListLengths :: Maybe [[Instruction]] -> [Int]
_instructionListLengths (Just []) = [0]
_instructionListLengths (Just [instructionList]) = [length instructionList]
_instructionListLengths (Just (instructionList:instructionLists)) =
  length instructionList : _instructionListLengths (Just instructionLists)
_instructionListLengths Nothing = []

_showInstructionList :: [[Instruction]] -> Int -> String
_showInstructionList [] _ = ""
_showInstructionList [instructions] depth =
  _showInstructions instructions depth
_showInstructionList (instructions:instructionsList) depth =
  _showInstructions instructions depth ++
  "\n" ++ _showInstructionList instructionsList depth

_showInstructions :: [Instruction] -> Int -> String
_showInstructions instructions depth =
  concatMap lambda instructions where lambda x = _showInstruction x depth

showInstructions :: [Instruction] -> IO()
showInstructions instructions = putStr (_showInstructions instructions 0)

_putOpCodeFromInstruction :: Instruction -> Put
_putOpCodeFromInstruction instruction =
  putWord8 (fromIntegral (fromEnum instruction))

_fputList :: (a -> Put) -> [a] -> Put
_fputList _ [] = _putString ""
_fputList func [element] = func element
_fputList func (element:elements) = func element >> _fputList func elements

_putString :: String -> Put
_putString string = let byteString = BSUTF8.fromString string
  in putInt32be (fromIntegral (BS.length byteString))
  >> putByteString byteString

_putInt32 :: Int -> Put
_putInt32 value = putInt32be (fromIntegral (value::Int))

_putBool :: Bool -> Put
_putBool bool = putWord8 (fromIntegral (fromEnum bool))

_compileInstruction :: Instruction -> Put
-- NoOp
_compileInstruction NoOp = _putOpCodeFromInstruction NoOp
-- PushI
_compileInstruction (PushI value) =
  _putOpCodeFromInstruction (PushI value) >> _putInt32 value
-- PushB
_compileInstruction (PushB bool) =
  _putOpCodeFromInstruction (PushB bool) >> _putBool bool
-- PushSym
_compileInstruction (PushSym symbolName Nothing) =
  _putOpCodeFromInstruction (PushSym symbolName Nothing)
  >> _putString symbolName
_compileInstruction (PushSym symbolName (Just symbolArgs)) =
  _fputList compileInstructions symbolArgs
  >> _putInt32 (length symbolArgs)
  >> _putOpCodeFromInstruction (PushSym symbolName (Just symbolArgs))
  >> _putString symbolName >> _putOpCodeFromInstruction Compiler.Call
-- PushStr
_compileInstruction (PushStr stringValue) =
  _putOpCodeFromInstruction (PushStr stringValue) >> _putString stringValue
-- PushList
_compileInstruction (PushList nbListValues listValues) =
  _putOpCodeFromInstruction (PushList nbListValues listValues)
  >> _putInt32 nbListValues >> _fputList compileInstructions listValues
-- PushArg
_compileInstruction (PushArg index) =
  _putOpCodeFromInstruction (PushArg index) >> _putInt32 index
-- Jump
_compileInstruction (Jump branchOffset) =
  _putOpCodeFromInstruction (Jump branchOffset)
  >> _putInt32 branchOffset
-- JumpIfFalse
_compileInstruction (JumpIfFalse branchOffset) =
  _putOpCodeFromInstruction (JumpIfFalse branchOffset)
  >> _putInt32 branchOffset
-- Add
_compileInstruction Add = _putOpCodeFromInstruction Add
-- Sub
_compileInstruction Sub = _putOpCodeFromInstruction Sub
-- Mul
_compileInstruction Mul = _putOpCodeFromInstruction Mul
-- Div
_compileInstruction Div = _putOpCodeFromInstruction Div
-- Mod
_compileInstruction Mod = _putOpCodeFromInstruction Mod
-- XorB
_compileInstruction XorB = _putOpCodeFromInstruction XorB
-- Eq
_compileInstruction Eq = _putOpCodeFromInstruction Eq
-- NotEq
_compileInstruction NotEq = _putOpCodeFromInstruction NotEq
-- Less
_compileInstruction Less = _putOpCodeFromInstruction Less
-- LessEq
_compileInstruction LessEq = _putOpCodeFromInstruction LessEq
-- Great
_compileInstruction Great = _putOpCodeFromInstruction Great
-- GreatEq
_compileInstruction GreatEq = _putOpCodeFromInstruction GreatEq
-- And
_compileInstruction And = _putOpCodeFromInstruction And
-- Or
_compileInstruction Or = _putOpCodeFromInstruction Or
-- Not
_compileInstruction Not = _putOpCodeFromInstruction Not
-- Then
_compileInstruction Then = _putOpCodeFromInstruction Then
-- ToStr
_compileInstruction ToStr = _putOpCodeFromInstruction ToStr
-- Neg
_compileInstruction Neg = _putOpCodeFromInstruction Neg
-- Call
_compileInstruction Compiler.Call = _putOpCodeFromInstruction Compiler.Call
-- Ret
_compileInstruction Ret = _putOpCodeFromInstruction Ret
-- Def
_compileInstruction (Def symbolName nbInstruction instructions)
  = _putOpCodeFromInstruction (Def symbolName nbInstruction instructions)
    >> _putString symbolName
    >> _putInt32 nbInstruction
    >> compileInstructions instructions
-- Fnv
_compileInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
  funcBodyInstructions nbArgsValuesInstructions
  (Just argsValuesInstructions)) =
    _putOpCodeFromInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
    funcBodyInstructions nbArgsValuesInstructions
    (Just argsValuesInstructions))
    >> _putInt32 nbArgsNames
    >> _fputList _putString argsNames
    >> _putInt32 nbFuncBodyInstructions
    >> _fputList _compileInstruction funcBodyInstructions
    >> _putInt32 (length nbArgsValuesInstructions)
    >> _fputList _putInt32 nbArgsValuesInstructions
    >> _fputList compileInstructions argsValuesInstructions
_compileInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
  funcBodyInstructions nbArgsValuesInstructions Nothing) =
    _putOpCodeFromInstruction (Fnv nbArgsNames argsNames nbFuncBodyInstructions
    funcBodyInstructions nbArgsValuesInstructions Nothing)
    >> _putInt32 nbArgsNames
    >> _fputList _putString argsNames
    >> _putInt32 nbFuncBodyInstructions
    >> _fputList _compileInstruction funcBodyInstructions
-- Cond
_compileInstruction (Compiler.Cond condInstructions
  nbTrueBlockInstructions trueBlockInstructions
  (Just falseBlockInstructions)) =
    _fputList _compileInstruction condInstructions
    >> _compileInstruction (JumpIfFalse nbTrueBlockInstructions)
    >> _fputList _compileInstruction trueBlockInstructions
    >> _fputList _compileInstruction falseBlockInstructions
_compileInstruction (Compiler.Cond condInstructions
  nbTrueBlockInstructions trueBlockInstructions Nothing) =
    _fputList _compileInstruction condInstructions
    >> _compileInstruction (JumpIfFalse nbTrueBlockInstructions)
    >> _fputList _compileInstruction trueBlockInstructions
-- Apnd
_compileInstruction Apnd = _putOpCodeFromInstruction Apnd
-- RemAllOcc
_compileInstruction RemAllOcc = _putOpCodeFromInstruction RemAllOcc
-- Get
_compileInstruction Get = _putOpCodeFromInstruction Get
-- Len
_compileInstruction Len = _putOpCodeFromInstruction Len

compileInstructions :: [Instruction] -> Put
compileInstructions = _fputList _compileInstruction

writeCompiledInstructionsToFile :: String -> Put -> IO()
writeCompiledInstructionsToFile filepath compiledInsts =
  BS.writeFile filepath (BS.concat $ BSL.toChunks $ runPut compiledInsts)

compile :: [Ast] -> String -> Bool -> IO()
compile ast filepath showInst = if showInst
  then showInstructions instructions
    >> writeCompiledInstructionsToFile filepath compiledInstructions
  else writeCompiledInstructionsToFile filepath compiledInstructions
  where
    instructions = concatMap astToInstructions ast ++ [Ret]
    compiledInstructions = _putInt32 (fromEnum MagicNumber) >> _fputList _compileInstruction instructions
