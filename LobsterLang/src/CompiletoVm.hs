{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- CompiletoVm
-}

module CompiletoVm (converte, makeConverte) where

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
    Right (remainingfile, _, 10) -> case (decodeOrFail remainingfile :: Either (BIN.ByteString, ByteOffset, String) (BIN.ByteString, ByteOffset, Int32)) of
        Left _ -> return ([], [], [])
        Right (remfile, _, val) -> converte remfile (env, arg, inst ++ [Push (IntVal (fromIntegral (val :: Int32) :: Int))] )
    Right (_, _, _) -> return (env, arg, inst)
