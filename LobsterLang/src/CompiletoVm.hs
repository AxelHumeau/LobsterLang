{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (convert, makeConvert, getString, getList, getDefinedValue, getFnv, getArg) where

import Compiler
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BIN
import GHC.Int
import Vm

type DcdStrInt = Either (BIN.ByteString, ByteOffset, String)
    (BIN.ByteString, ByteOffset, Int32)
type DcdStrWord8 = Either (BIN.ByteString, ByteOffset, String)
    (BIN.ByteString, ByteOffset, Word8)
type DcdStrChar = Either (BIN.ByteString, ByteOffset, String)
    (BIN.ByteString, ByteOffset, Char)

makeConvert :: String -> IO Inst
makeConvert path = BIN.readFile path >>= \filepath ->
    case (decodeOrFail filepath :: DcdStrInt) of
      Left _ -> return []
      Right (allfile, _, magicNumber)
        | (fromIntegral (magicNumber :: Int32) :: Int)
            == fromEnum MagicNumber -> convert allfile []
        | otherwise -> return []

convert :: BIN.ByteString -> Inst -> IO Inst
convert file inst =
  case (decodeOrFail file :: DcdStrWord8) of
    Left _ -> return inst
    Right (remainingFile, _, opcode) ->
      convertInstruction remainingFile inst (toEnum (fromIntegral opcode))

convertInstruction :: BIN.ByteString -> Inst -> Compiler.Instruction -> IO Inst
convertInstruction remainingFile inst NoOp = convert remainingFile inst
convertInstruction remainingFile inst (PushI _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, val) ->
      convert remfile
        (inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))])
convertInstruction remainingFile inst (PushB _) =
  case (decodeOrFail remainingFile :: DcdStrWord8) of
    Left _ -> return []
    Right (remfile, _, 1) -> convert remfile (inst ++ [Push (BoolVal True)])
    Right (remfile, _, 0) -> convert remfile (inst ++ [Push (BoolVal False)])
    Right (remfile, _, _) -> convert remfile inst
convertInstruction remainingFile inst (PushStr _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, byteToRead) -> convert (snd (getString
        (fromIntegral (byteToRead :: Int32) :: Int) remfile []))
        (inst ++ [Push (StringVal (fst (getString (fromIntegral
            (byteToRead :: Int32) :: Int) remfile [])))])
convertInstruction remainingFile inst (PushSym _ _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, byteToRead) ->
      convert (snd (getString (fromIntegral (byteToRead :: Int32) :: Int)
        remfile [])) (inst ++ [ PushEnv (fst (getString
            (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
convertInstruction remainingFile inst (Compiler.PushArg _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, val) ->
      convert remfile
        (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
convertInstruction remainingFile inst (Compiler.Jump _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, val) ->
      convert remfile (inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
convertInstruction remainingFile inst (Compiler.JumpIfFalse _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, val) ->
      convert remfile
        (inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
----------------------------------------------------------------
convertInstruction remainingFile inst (Compiler.Def {}) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, val) ->
      convert reminfile (inst ++ symbolValue ++ symbolName)
      where
        remainAfterStr = snd (getString (fromIntegral
            (val :: Int32) :: Int ) remfile [])
        symbolName = [Vm.Define (fst (getString (fromIntegral
            (val :: Int32) :: Int ) remfile []))]
        nbinstructions = case (decodeOrFail remainAfterStr :: DcdStrInt) of
          Left _ -> 0
          Right (_, _, nbinst) -> (fromIntegral (nbinst :: Int32) :: Int)
        fileAfternbinst = case (decodeOrFail remainAfterStr :: DcdStrInt) of
          Left _ -> remainAfterStr
          Right (rema, _, _) -> rema
        symbolValue = fst (getDefinedValue nbinstructions fileAfternbinst [])
        reminfile = snd (getDefinedValue nbinstructions fileAfternbinst [])
convertInstruction remainingFile inst (Compiler.Fnv {}) =
  convert
    (snd (getFnv (-1) remainingFile []))
    (inst ++ fst (getFnv (-1) remainingFile []))
convertInstruction remainingFile inst Compiler.Call =
  convert remainingFile (inst ++ [Vm.Call])
convertInstruction remainingFile inst Compiler.Ret =
  convert remainingFile (inst ++ [Vm.Ret])
convertInstruction remainingFile inst Compiler.Add =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
convertInstruction remainingFile inst Compiler.Sub =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
convertInstruction remainingFile inst Compiler.Mul =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
convertInstruction remainingFile inst Compiler.Div =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
convertInstruction remainingFile inst Compiler.Mod =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
convertInstruction remainingFile inst Compiler.Eq =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
convertInstruction remainingFile inst Compiler.Less =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
convertInstruction remainingFile inst Compiler.LessEq =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
convertInstruction remainingFile inst Compiler.Great =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
convertInstruction remainingFile inst Compiler.GreatEq =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
convertInstruction remainingFile inst Compiler.And =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
convertInstruction remainingFile inst Compiler.Or =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
convertInstruction remainingFile inst Compiler.XorB =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Xorb)])
convertInstruction remainingFile inst Compiler.Not =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
convertInstruction remainingFile inst Compiler.ToStr =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
convertInstruction remainingFile inst Compiler.Apnd =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
convertInstruction remainingFile inst Compiler.RemAllOcc =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
convertInstruction remainingFile inst Compiler.Get =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
convertInstruction remainingFile inst Compiler.Len =
  convert remainingFile (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
convertInstruction remainingFile inst Compiler.PutArg =
  convert remainingFile (inst ++ [Vm.PutArg])
convertInstruction remainingFile inst Compiler.Neg =
  convert remainingFile inst
convertInstruction remainingFile inst (Compiler.PushList _ _) =
  case (decodeOrFail remainingFile :: DcdStrInt) of
    Left _ -> return []
    Right (remfile, _, lenList) ->
      convert (snd (getList (fromIntegral
      (lenList :: Int32) :: Int) remfile []))
      (inst ++ fst (getList (fromIntegral (lenList :: Int32) :: Int)
      remfile []) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
convertInstruction remainingFile inst _ =
  convert remainingFile inst

getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s =
  case (decodeOrFail byteString :: DcdStrChar) of
    Right (remainingFile, _, a) ->
      getString (nbytes - 1) remainingFile (s ++ [a])
    Left _ -> (s, byteString)

getFnv ::
  Int ->
  BIN.ByteString ->
  [Vm.Instruction] ->
  ([Vm.Instruction], BIN.ByteString)
getFnv 0 byteString inst = (inst, byteString)
-- start
getFnv (-1) byteString inst =
  case (decodeOrFail byteString :: DcdStrInt) of
    Left _ -> (inst, byteString)
    Right (nByteString, _, val) ->
      getFnv 0 (snd (getInstructionFunc nbinstruction
      byteStringafterNbInst []) ) (inst ++ [ Vm.Push (Vm.Function (fst
      (getInstructionFunc nbinstruction byteStringafterNbInst [] ) )
      (fromIntegral (val :: Int32) :: Int))])
      where
        nbinstruction = case (decodeOrFail nByteString :: DcdStrInt) of
          Left _ -> 0
          Right (_, _, valu) -> (fromIntegral (valu :: Int32) :: Int)
        byteStringafterNbInst = case (decodeOrFail nByteString :: DcdStrInt) of
          Left _ -> nByteString
          Right (afterNbInst, _, _) -> afterNbInst
getFnv _ byteString inst = (inst, byteString)

getArg :: Int -> BIN.ByteString -> [Vm.Instruction] ->
    ([Vm.Instruction], BIN.ByteString)
getArg 0 byteString inst = (inst, byteString)
getArg nbInstruction byteString inst =
    case (decodeOrFail byteString :: DcdStrWord8) of
    Left _ -> ([], byteString)
    Right (remainingFile, _, opcode) ->
      getArgFromInstruction nbInstruction byteString remainingFile inst
      (toEnum (fromIntegral opcode))

getArgFromInstruction :: Int -> BIN.ByteString -> BIN.ByteString ->
    [Vm.Instruction] ->
    Compiler.Instruction -> ([Vm.Instruction], BIN.ByteString)
getArgFromInstruction nbInstruction byteString remainingFile inst (PushI _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
  Left _ -> ([], byteString)
  Right (remfile, _, val) -> getArg (nbInstruction - 1)
    remfile (inst ++ [Vm.Push (IntVal
    (fromIntegral (val :: Int32) :: Int))])
getArgFromInstruction nbInstruction byteString remainingFile inst (PushB _) =
    case (decodeOrFail remainingFile :: DcdStrWord8) of
  Left _ -> (inst, byteString)
  Right (remfile, _, 1) -> getArg (nbInstruction - 1)
    remfile (inst ++ [Vm.Push (BoolVal True)])
  Right (remfile, _, 0) -> getArg (nbInstruction - 1)
    remfile (inst ++ [Vm.Push (BoolVal False)])
  Right (_, _, _) -> (inst, byteString)
getArgFromInstruction nbInstruction byteString
    remainingFile inst (Compiler.PushStr _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
  Left _ -> (inst, byteString)
  Right (remfile, _, byteToRead) -> getArg (nbInstruction - 1)
    (snd (getString (fromIntegral (byteToRead :: Int32) :: Int)
    remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString
    (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
getArgFromInstruction nbInstruction byteString remainingFile inst
    (Compiler.PushSym _ _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
  Left _ -> (inst, byteString)
  Right (remfile, _, byteToRead) -> getArg (nbInstruction - 1)
    (snd (getString (fromIntegral (byteToRead :: Int32) :: Int)
    remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral
    (byteToRead :: Int32) :: Int) remfile []))])
getArgFromInstruction nbInstruction byteString
    remainingFile inst (Compiler.PushList _ _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
  Left _ -> ([], byteString)
  Right (remfile, _, lenList) -> getArg (nbInstruction - 1)
    (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile []))
    (inst ++ fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])
    ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
getArgFromInstruction nbInstruction _ remainingFile
    inst (Compiler.PushArg _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
        Left _ -> ([], remainingFile)
        Right (remfile, _, val) -> getArg (nbInstruction - 1) remfile
          (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
getArgFromInstruction nbInstruction _ remainingFile inst Compiler.PutArg =
    getArg (nbInstruction - 1) remainingFile (inst ++ [Vm.PutArg])
getArgFromInstruction nbInstruction _ remainingFile inst (Compiler.Fnv {}) =
    getArg (nbInstruction - 1) (snd (getFnv (-1) remainingFile []))
    (inst ++ fst (getFnv (-1) remainingFile []))
getArgFromInstruction _ byteString _ inst _ =
    (inst, byteString)

getInstructionFunc :: Int -> BIN.ByteString -> [Vm.Instruction] ->
    ([Vm.Instruction], BIN.ByteString)
getInstructionFunc 0 byteString inst = (inst, byteString)
getInstructionFunc nbInstruction byteString inst =
    case (decodeOrFail byteString :: DcdStrWord8) of
  Left _ -> ([], byteString)
  Right (remainingFile, _, opcode) ->
    getInstFnvFromInst nbInstruction byteString inst
        remainingFile (toEnum (fromIntegral opcode))

getInstFnvFromInst :: Int -> BIN.ByteString -> [Vm.Instruction] ->
    BIN.ByteString -> Compiler.Instruction ->
    ([Vm.Instruction], BIN.ByteString)
getInstFnvFromInst nbInstruction byteString inst remainingFile (PushI _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], byteString)
      Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int))])
getInstFnvFromInst nbInstruction byteString inst remainingFile (PushB _) =
    case (decodeOrFail remainingFile :: DcdStrWord8) of
      Left _ -> (inst, byteString)
      Right (remfile, _, 1) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.Push (BoolVal True)])
      Right (remfile, _, 0) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.Push (BoolVal False)])
      Right (_, _, _) -> (inst, byteString)
getInstFnvFromInst nbInstruction byteString inst
    remainingFile (Compiler.PushStr _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getInstructionFunc (nbInstruction - 1)
        (snd (getString (fromIntegral (byteToRead :: Int32) :: Int)
        remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString
        (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
getInstFnvFromInst nbInstruction byteString inst
    remainingFile (Compiler.PushSym _ _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getInstructionFunc (nbInstruction - 1)
        (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile
        [])) (inst ++ [PushEnv (fst (getString (fromIntegral
        (byteToRead :: Int32) :: Int) remfile []))])
getInstFnvFromInst nbInstruction _ inst remainingFile (Compiler.PushList _ _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, lenList) -> getInstructionFunc (nbInstruction - 1)
        (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile []))
        (inst ++ fst (getList (fromIntegral (lenList :: Int32) :: Int)
        remfile []) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
getInstFnvFromInst nbInstruction _ inst remainingFile (Compiler.PushArg _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
getInstFnvFromInst nbInstruction _ inst remainingFile (Compiler.Jump _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
getInstFnvFromInst nbInstruction _ inst remainingFile (Compiler.JumpIfFalse _) =
    case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile
        (inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Add =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Sub =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Mul =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Div =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Mod =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Eq =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Less =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.LessEq =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Great =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.GreatEq =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.And =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Or =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.XorB =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Xorb)])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Not =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.ToStr =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Apnd =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.RemAllOcc =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Get =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Len =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.PutArg =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.PutArg])
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Ret =
    getInstructionFunc (nbInstruction - 1) remainingFile
    (inst ++ [Vm.Ret])
getInstFnvFromInst nbInstruction _ inst remainingFile (Compiler.Fnv {}) =
    getInstructionFunc (nbInstruction - 1)
    (snd (getFnv (-1) remainingFile []))
    (inst ++ fst (getFnv (-1) remainingFile []))
getInstFnvFromInst nbInstruction _ inst remainingFile Compiler.Call =
    getInstructionFunc (nbInstruction - 1) remainingFile (inst ++ [Vm.Call])
getInstFnvFromInst _ byteString inst _ _ = (inst, byteString)

getDefinedValue :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getDefinedValue 0 byteString inst = (inst, byteString)
getDefinedValue nbInstruction byteString inst = case (decodeOrFail byteString :: DcdStrWord8) of
  Left _ -> ([], byteString)
  Right (remainingFile, _, opcode) -> case toEnum (fromIntegral opcode) of
    PushI _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], byteString)
      Right (remfile, _, val) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int))])
    PushB _ -> case (decodeOrFail remainingFile :: DcdStrWord8) of
      Left _ -> (inst, byteString)
      Right (remfile, _, 1) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
      Right (remfile, _, 0) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
      Right (_, _, _) -> (inst, byteString)
    Compiler.PushStr _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getDefinedValue (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
    Compiler.PushSym _ _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getDefinedValue (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
    Compiler.PushList _ _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, lenList) -> getDefinedValue (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
    Compiler.PushArg _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, val) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
    Compiler.Add -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
    Compiler.Sub -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
    Compiler.Mul -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
    Compiler.Div -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
    Compiler.Mod -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
    Compiler.Eq -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
    Compiler.Less -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
    Compiler.LessEq -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
    Compiler.Great -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
    Compiler.GreatEq -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
    Compiler.And -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
    Compiler.Or -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
    Compiler.XorB -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Xorb)])
    Compiler.Not -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
    Compiler.ToStr -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
    Compiler.Apnd -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
    Compiler.RemAllOcc -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
    Compiler.Get -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
    Compiler.Len -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
    Compiler.PutArg -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.PutArg])
    Compiler.Ret -> getDefinedValue (nbInstruction - 1) remainingFile (inst ++ [Vm.Ret])
    Compiler.Fnv {} -> getDefinedValue (nbInstruction - 1) (snd (getFnv (-1) remainingFile [])) (inst ++ fst (getFnv (-1) remainingFile []))
    _ -> (inst, byteString)

getList :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getList 0 byteString inst = (inst, byteString)
getList nbInstruction byteString inst = case (decodeOrFail byteString :: DcdStrWord8) of
  Left _ -> ([], byteString)
  Right (remainingFile, _, opcode) -> case toEnum (fromIntegral opcode) of
    PushI _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], byteString)
      Right (remfile, _, val) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int))])
    PushB _ -> case (decodeOrFail remainingFile :: DcdStrWord8) of
      Left _ -> (inst, byteString)
      Right (remfile, _, 1) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
      Right (remfile, _, 0) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
      Right (_, _, _) -> (inst, byteString)
    Compiler.PushStr _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getList (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
    Compiler.PushSym _ _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> (inst, byteString)
      Right (remfile, _, byteToRead) -> getList (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
    Compiler.PushList _ _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, lenList) -> getList (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
    Compiler.PushArg _ -> case (decodeOrFail remainingFile :: DcdStrInt) of
      Left _ -> ([], remainingFile)
      Right (remfile, _, val) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
    _ -> (inst, byteString)
