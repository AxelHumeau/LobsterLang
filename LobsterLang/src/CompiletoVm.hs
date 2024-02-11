{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (convert, makeConvert, getString, getList, getDefinedValue, getFnv, getArg) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BIN
import GHC.Int
import Vm
import Compiler

makeConvert :: String -> IO Inst
makeConvert path = BIN.readFile path >>= \filepath -> case (decodeOrFail filepath :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    Left _ -> return []
    Right (allfile, _, magicNumber)
        | (fromIntegral (magicNumber :: Int32) :: Int) == fromEnum MagicNumber -> convert allfile []
        | otherwise -> return []

convert :: BIN.ByteString -> Inst -> IO Inst
convert file inst = case (decodeOrFail file :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
        Left _ -> return inst
        Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
            NoOp -> convert remainingfile inst
            PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, val) -> convert remfile (inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))])
            PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
                Left _ -> return []
                Right (remfile, _, 1) -> convert remfile (inst ++ [Push (BoolVal True)])
                Right (remfile, _, 0) -> convert remfile (inst ++ [Push (BoolVal False)])
                Right (remfile, _, _) -> convert remfile inst
            PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, byteToRead) -> convert (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
            PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, byteToRead) -> convert (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
            Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, val) -> convert remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
            Compiler.Jump _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, val) -> convert remfile (inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
            Compiler.JumpIfFalse _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, val) -> convert remfile (inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
                ----------------------------------------------------------------
            Compiler.Def {} -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, val) -> convert reminfile (inst ++ symbolValue ++ symbolName)
                    where
                        remainAfterStr = snd (getString (fromIntegral (val :: Int32) :: Int) remfile [])
                        symbolName = [Vm.Define (fst (getString (fromIntegral (val :: Int32) :: Int) remfile []))]
                        nbinstructions = case (decodeOrFail remainAfterStr :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                            Left _ -> 0
                            Right (_ , _, nbinst) -> (fromIntegral (nbinst :: Int32) :: Int)
                        fileAfternbinst = case (decodeOrFail remainAfterStr :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                            Left _ -> remainAfterStr
                            Right (rema, _, _) -> rema
                        symbolValue = fst (getDefinedValue nbinstructions fileAfternbinst [])
                        reminfile = snd (getDefinedValue nbinstructions fileAfternbinst [])
            Compiler.Fnv {} -> convert (snd (getFnv (-1) remainingfile [])) (inst ++ fst (getFnv (-1) remainingfile []))
            Compiler.Call -> convert remainingfile (inst ++ [Vm.Call])
            Compiler.Ret -> convert remainingfile (inst ++ [Vm.Ret])
            Compiler.Add ->  convert remainingfile (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
            Compiler.Sub -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
            Compiler.Mul -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
            Compiler.Div -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
            Compiler.Mod -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
            Compiler.Eq -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
            Compiler.Less -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
            Compiler.LessEq -> convert remainingfile (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
            Compiler.Great -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
            Compiler.GreatEq -> convert remainingfile (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
            Compiler.And -> convert remainingfile (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
            Compiler.Or ->convert remainingfile (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
            Compiler.XorB -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Xorb)])
            Compiler.Not -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
            Compiler.ToStr -> convert remainingfile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
            Compiler.Apnd -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
            Compiler.RemAllOcc -> convert remainingfile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
            Compiler.Get -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
            Compiler.Len -> convert remainingfile (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
            Compiler.PutArg -> convert remainingfile (inst ++ [Vm.PutArg])
            Compiler.Neg -> convert remainingfile inst
            Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                Left _ -> return []
                Right (remfile, _, lenList) -> convert (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [] )) (inst ++ fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile []) ++  [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
            _ -> convert remainingfile inst

getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Char)) of
    Right (remainingfile, _, a) -> getString (nbytes - 1) remainingfile (s ++ [a])
    Left _ -> (s, byteString)

getFnv :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getFnv 0 byteString inst = (inst, byteString)
-- start
getFnv (-1) byteString inst = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        Left _ -> (inst, byteString)
        Right (nByteString, _, val) -> (getFnv 0 byteStringAfterInst (inst ++ [Vm.Push (Vm.Function functionInstruction (fromIntegral (val :: Int32) :: Int))]))
            where
                nbinstruction = case (decodeOrFail nByteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                    Left _ -> 0
                    Right (_, _, valu) -> (fromIntegral (valu :: Int32) :: Int)
                byteStringafterNbInst = case (decodeOrFail nByteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
                    Left _ -> nByteString
                    Right (afterNbInst, _, _) -> afterNbInst
                functionInstruction = fst (getInstructionFunc nbinstruction byteStringafterNbInst [])
                byteStringAfterInst = snd (getInstructionFunc nbinstruction byteStringafterNbInst [])
getFnv _ byteString inst = (inst, byteString)


getArg :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getArg 0 byteString inst = (inst, byteString)
getArg nbInstruction byteString inst = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> ([], byteString)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], byteString)
            Right (remfile, _, val) -> getArg (nbInstruction - 1) remfile (inst ++ [(Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int)))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, 1) -> getArg (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
            Right (remfile, _, 0) -> getArg (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
            Right (_, _, _) -> (inst, byteString)
        Compiler.PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getArg (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        Compiler.PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getArg (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
        Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], byteString)
            Right (remfile, _, lenList) -> getArg (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
        Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getArg (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
        Compiler.PutArg -> getArg (nbInstruction - 1) remainingfile (inst ++ [Vm.PutArg])
        Compiler.Fnv {} -> getArg (nbInstruction - 1) (snd (getFnv (-1) remainingfile [])) (inst ++ (fst (getFnv (-1) remainingfile [])))
        _ -> (inst, byteString)

getInstructionFunc :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getInstructionFunc 0 byteString inst = (inst, byteString)
getInstructionFunc nbInstruction byteString inst = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> ([], byteString)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        PushI _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], byteString)
            Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, 1) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
            Right (remfile, _, 0) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
            Right (_, _, _) -> (inst, byteString)
        Compiler.PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getInstructionFunc (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        Compiler.PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getInstructionFunc (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
        Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, lenList) -> getInstructionFunc (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
        Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
        Compiler.Jump _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
        Compiler.JumpIfFalse _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getInstructionFunc (nbInstruction - 1) remfile (inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
        Compiler.Add ->  getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
        Compiler.Sub -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
        Compiler.Mul -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
        Compiler.Div -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
        Compiler.Mod -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
        Compiler.Eq -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
        Compiler.Less -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
        Compiler.LessEq -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
        Compiler.Great -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
        Compiler.GreatEq -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
        Compiler.And -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
        Compiler.Or ->getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
        Compiler.XorB -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Xorb)])
        Compiler.Not -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
        Compiler.ToStr -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
        Compiler.Apnd -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
        Compiler.RemAllOcc -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
        Compiler.Get -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
        Compiler.Len -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
        Compiler.PutArg -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.PutArg])
        Compiler.Ret -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Ret])
        Compiler.Fnv {} -> getInstructionFunc (nbInstruction - 1) (snd (getFnv (-1) remainingfile [])) (inst ++ (fst (getFnv (-1) remainingfile [])))
        Compiler.Call -> getInstructionFunc (nbInstruction - 1) remainingfile (inst ++ [Vm.Call])
        _ -> (inst, byteString)

getDefinedValue :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getDefinedValue 0 byteString inst = (inst, byteString)
getDefinedValue nbInstruction byteString inst = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> ([], byteString)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], byteString)
            Right (remfile, _, val) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [(Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int)))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, 1) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
            Right (remfile, _, 0) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
            Right (_, _, _) -> (inst, byteString)
        Compiler.PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getDefinedValue (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        Compiler.PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getDefinedValue (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
        Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, lenList) -> getDefinedValue (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
        Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getDefinedValue (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
        Compiler.Add ->  getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
        Compiler.Sub -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
        Compiler.Mul -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
        Compiler.Div -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
        Compiler.Mod -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
        Compiler.Eq -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
        Compiler.Less -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
        Compiler.LessEq -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
        Compiler.Great -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
        Compiler.GreatEq -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
        Compiler.And -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.And), Vm.Call])
        Compiler.Or ->getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
        Compiler.XorB -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Xorb)])
        Compiler.Not -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
        Compiler.ToStr -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
        Compiler.Apnd -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
        Compiler.RemAllOcc -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
        Compiler.Get -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
        Compiler.Len -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Len), Vm.Call])
        Compiler.PutArg -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.PutArg])
        Compiler.Ret -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Ret])
        Compiler.Fnv {} -> getDefinedValue (nbInstruction - 1) (snd (getFnv (-1) remainingfile [])) (inst ++ (fst (getFnv (-1) remainingfile [])))
        _ -> (inst, byteString)

getList :: Int -> BIN.ByteString -> [Vm.Instruction] -> ([Vm.Instruction], BIN.ByteString)
getList 0 byteString inst = (inst, byteString)
getList nbInstruction byteString inst = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> ([], byteString)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], byteString)
            Right (remfile, _, val) -> getList (nbInstruction - 1) remfile (inst ++ [(Vm.Push (IntVal (fromIntegral (val :: Int32) :: Int)))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, 1) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal True)])
            Right (remfile, _, 0) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.Push (BoolVal False)])
            Right (_, _, _) -> (inst, byteString)
        Compiler.PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getList (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [Vm.Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        Compiler.PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> (inst, byteString)
            Right (remfile, _, byteToRead) -> getList (nbInstruction - 1) (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
        Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, lenList) -> getList (nbInstruction - 1) (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) (inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++ [Vm.PushList (fromIntegral (lenList :: Int32) :: Int)])
        Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> ([], remainingfile)
            Right (remfile, _, val) -> getList (nbInstruction - 1) remfile (inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
        _ -> (inst, byteString)
