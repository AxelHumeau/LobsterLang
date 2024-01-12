{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (convert, makeConvert, getString) where

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
        Compiler.Jump _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
        Compiler.JumpIfFalse _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> convert remfile (env, arg, inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
        -- Compiler.Def -> -- TODO:
        -- Right (remainingfile, _, 41) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO: idk
        Compiler.Call -> convert remainingfile (env, arg, inst ++ [Vm.Call])
        Compiler.Ret -> convert remainingfile (env, arg, inst ++ [Vm.Ret])
        Compiler.Add ->  convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Add)])
        Compiler.Sub -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Sub)])
        Compiler.Mul -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Mul)])
        Compiler.Div -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Div)])
        Compiler.Mod -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Mod)])
        Compiler.Eq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Eq)])
        Compiler.Less -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Less)])
        Compiler.LessEq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.LessEq)])
        Compiler.Great -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Great)])
        Compiler.GreatEq -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.GreatEq)])
        Compiler.And -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.And)])
        Compiler.Or ->convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Or)])
        Compiler.Not -> convert remainingfile (env, arg, inst ++ [Vm.Push (Op Vm.Not)])
        Compiler.Neg -> convert remainingfile (env, arg, inst)
    Right (remainingfile, _, _) -> convert remainingfile (env, arg, inst)


getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Char)) of
    Right (remainingfile, _, a) -> getString (nbytes - 1) remainingfile (s ++ [a])
    Left _ -> (s, byteString)
