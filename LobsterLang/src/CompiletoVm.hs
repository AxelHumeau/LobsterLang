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

makeConverte :: String -> IO (Env, Arg, Inst)
makeConverte path = BIN.readFile path >>= \filepath -> converte filepath ([], [], [])

converte :: BIN.ByteString -> (Env, Arg, Inst) -> IO (Env, Arg, Inst)
converte file (env, arg, inst) = case (decodeOrFail file :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
    Left _ -> return (env, arg, inst)
    Right (remainingfile, _, 0) -> converte remainingfile (env, arg, inst)
    Right (remainingfile, _, 10) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        Left _ -> return ([], [], [Ret])
        Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))])
    Right (remainingfile, _, 11) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Word8)) of
        Left _ -> return ([], [], [Call])
        Right (remfile, _, 1) -> converte remfile (env, arg, inst ++ [Push (BoolVal True)])
        Right (remfile, _, 0) -> converte remfile (env, arg, inst ++ [Push (BoolVal False)])
        Right (remfile, _, _) -> converte remfile (env, arg, inst)
    Right (remainingfile, _, 12) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        Left _ -> return ([], [], [Ret, Call])
        Right (remfile, _, byteToRead) -> converte (snd (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])) (env, arg, inst ++ [Push (StringVal (fst (getString (fromIntegral (byteToRead :: Int32) :: Int) remfile [])))])
    Right (remainingfile, _, 30) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        Left _ -> return ([], [], [])
        Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Jump (fromIntegral (val :: Int32) :: Int)]) -- TODO:
    Right (remainingfile, _, _) -> converte remainingfile (env, arg, inst)
    -- Right (remainingfile, _, 31) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 40) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 41) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 42) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 43) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 45) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 50) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 51) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 52) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 53) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 54) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 60) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 61) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 62) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 63) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 64) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 70) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 71) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    -- Right (remainingfile, _, 72) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:
    --  Right (remainingfile, _, 80) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
    --     Left _ -> return ([], [], [])
    --     Right (remfile, _, val) -> -- TODO:


getString :: Int -> BIN.ByteString -> String -> (String, BIN.ByteString)
getString 0 byteString str = (str, byteString)
getString nbytes byteString s = case (decodeOrFail byteString :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Char)) of
    Right (remainingfile, _, a) -> getString (nbytes - 1) remainingfile (s ++ [a])
    Left _ -> (s, byteString)
