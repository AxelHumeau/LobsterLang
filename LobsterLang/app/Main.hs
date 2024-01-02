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
import Data.Maybe (fromMaybe)
import Debug.Trace


-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
inputLoop new = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case parseTest line new of
        (Nothing, stack) -> (if stack == new then print "***ERROR" >> exitWith (ExitFailure 84) else inputLoop stack)
        -- (Just res, stack') -> print res >> writeCompiledAstToFile "test" (compileAst res)
        (Just res, stack') -> print res >> let instructions = (astToInstructions res) in showInstructions instructions

-- | Main
main :: IO ()
main = do
    -- let c = "(define foo 21)\n(define x 5)\n(define value (* x foo))"
    -- let ast = parseLisp c []
    -- putStrLn ("VAL" ++ show 5)
    -- putStrLn (compileAst (fst ast))
    print "Start of Interpretation Lisp" >> inputLoop []

