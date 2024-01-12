{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (converte, makeConverte, getString) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BIN
import GHC.Int
import Vm
import Compiler

makeConverte :: String -> IO (Env, Arg, Inst)
makeConverte path = BIN.readFile path >>= \filepath -> converte filepath ([], [], [])

converte :: BIN.ByteString -> (Env, Arg, Inst) -> IO (Env, Arg, Inst)
converte file (env, arg, inst) = case (decodeOrFail file :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> return (env, arg, inst)
    Right (remainingfile, _, opcode) -> case toEnum (fromIntegral opcode) of
        NoOp -> converte remainingfile (env, arg, inst)
        PushI _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))])
        PushB _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, 1) -> converte remfile (env, arg, inst ++ [Push (BoolVal True)])
            Right (remfile, _, 0) -> converte remfile (env, arg, inst ++ [Push (BoolVal False)])
            Right (remfile, _, _) -> converte remfile (env, arg, inst)
        PushS _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, byteToRead) -> converte (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (env, arg, inst ++ [Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
        Compiler.Jump _-> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Vm.Jump (fromIntegral (val :: Int32) :: Int)])
        Compiler.JumpIfFalse _ -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
            Left _ -> return ([], [], [])
            Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Vm.JumpIfFalse (fromIntegral (val :: Int32) :: Int)])
        -- Right (remainingfile, _, 40) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO: funcion ?
        -- Right (remainingfile, _, 41) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO: idk
        Compiler.Call -> converte remainingfile (env, arg, inst ++ [Vm.Call])
        Compiler.Ret -> converte remainingfile (env, arg, inst ++ [Vm.Ret])
        -- Right (remainingfile, _, 45) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO: Condition ?
        -- Compiler.Add -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Sub -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Mul -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Div -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Mod -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Eq -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Less -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.LessEq -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Great -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.GreatEq -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Add -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Or -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Not -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
        -- Compiler.Neg -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        --     Left _ -> return ([], [], [])
        --     Right (remfile, _, val) -> -- TODO:
    Right (remainingfile, _, _) -> converte remainingfile (env, arg, inst)


getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Char)) of
    Right (remainingfile, _, a) -> getString (nbytes - 1) remainingfile (s ++ [a])
    Left _ -> (s, byteString)
