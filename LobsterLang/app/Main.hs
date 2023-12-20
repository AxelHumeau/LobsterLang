{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Main
-}

module Main (main) where

import Parse
import Scope
import System.IO (isEOF)
import System.Exit (exitWith, ExitCode (ExitFailure))

import Compiler
import qualified AST


-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
inputLoop new = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case parseLisp line new of
        (Nothing, stack) -> (if stack == new then print "***ERROR" >> exitWith (ExitFailure 84) else inputLoop stack)
        (res, stack') -> print res >> inputLoop stack'

-- | Main
main :: IO ()
main =
    -- putStrLn ("VAL" ++ show 5)
    putStrLn (compileAst (AST.Call "myFunc" [(AST.Value 5), (AST.Value 5), (AST.Boolean True), (AST.Symbol "is_neg")]))
    -- print "Start of Interpretation Lisp" >> inputLoop []

-- main :: IO ()
-- main = do
--     BL.writeFile "out" (BLU.pack "test")
