{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (convert, makeConvert, getString, getList, getDefinedValue) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BIN
import GHC.Int
import Vm
import Compiler

makeConvert :: String -> IO (Env, Arg, Inst)
makeConvert path = BIN.readFile path >>= \filepath -> convert filepath ([], [], [])

convert :: BIN.ByteString -> (Env, Arg, Inst) -> IO (Env, Arg, Inst)
convert file (env, arg, inst) = case (decodeOrFail file :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> return (env, arg, inst)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        NoOp -> convert remainingfile (env, arg, inst)
        PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, 1) -> convert remfile (env, arg, inst ++ [Push (BoolVal True)])
            Right (remfile, _, 0) -> convert remfile (env, arg, inst ++ [Push (BoolVal False)])
            Right (remfile, _, _) -> convert remfile (env, arg, inst)
        PushStr _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, byteToRead) -> convert (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (env, arg, inst ++ [Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        PushSym _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, byteToRead) -> convert (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (env, arg, inst ++ [PushEnv (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile []))])
        Compiler.PushArg _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Vm.PushArg (fromIntegral (val :: Int32) :: Int)])
        Compiler.Jump _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
        Compiler.JumpIfFalse _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
            ----------------------------------------------------------------
        Compiler.Def _ _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert reminfile (env, arg, inst ++ symbolValue ++ symbolName)
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
        -- nbnamearg -> namearg -> nbinstructions -> inst -> nbvalue -> value
        -- Compiler.Fnv ->  -- fnv
        Compiler.Call -> convert remainingfile (env, arg, inst ++ [Vm.Call])
        Compiler.Ret -> convert remainingfile (env, arg, inst ++ [Vm.Ret])
        Compiler.Add ->  convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Add), Vm.Call])
        Compiler.Sub -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Sub), Vm.Call])
        Compiler.Mul -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Mul), Vm.Call])
        Compiler.Div -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Div), Vm.Call])
        Compiler.Mod -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Mod), Vm.Call])
        Compiler.Eq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Eq), Vm.Call])
        Compiler.Less -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Less), Vm.Call])
        Compiler.LessEq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.LessEq), Vm.Call])
        Compiler.Great -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Great), Vm.Call])
        Compiler.GreatEq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.GreatEq), Vm.Call])
        Compiler.And -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.And), Vm.Call])
        Compiler.Or ->convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Or), Vm.Call])
        Compiler.Not -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
        Compiler.ToStr -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
        Compiler.Apnd -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
        Compiler.RemAllOcc -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
        Compiler.Get -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
        Compiler.Neg -> convert remainingfile (env, arg, inst)
        Compiler.PushList _ _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, lenList) -> convert (snd (getList (fromIntegral (lenList :: Int32) :: Int) remfile [] )) (env, arg, inst ++ (fst (getList (fromIntegral (lenList :: Int32) :: Int) remfile [])) ++  [(Vm.PushList (fromIntegral (lenList :: Int32) :: Int))])
        _ -> convert remainingfile (env, arg, inst)

getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Char)) of
    Right (remainingfile, _, a) -> getString (nbytes - 1) remainingfile (s ++ [a])
    Left _ -> (s, byteString)

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
        Compiler.Not -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Not), Vm.Call])
        Compiler.ToStr -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.ToString), Vm.Call])
        Compiler.Apnd -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Append), Vm.Call])
        Compiler.RemAllOcc -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.RmOcc), Vm.Call])
        Compiler.Get -> getDefinedValue (nbInstruction - 1) remainingfile (inst ++ [Vm.Push (Op Vm.Get), Vm.Call])
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
        _ -> (inst, byteString)
