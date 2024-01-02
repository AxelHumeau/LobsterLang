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

-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
inputLoop new = isEOF >>= \end -> if end then putStrLn "End of Interpretation GLaDOS" else
    getLine >>= \line -> case parseLisp line new of
        (Left err, _) -> putStrLn ("\ESC[31m\ESC[1mThe lobster is angry: " ++ err ++ "\ESC[0m") >> inputLoop new
        (Right Nothing, stack) -> inputLoop stack
        (Right (Just res), stack') -> print res >> inputLoop stack'
        -- Compile (Just res, stack') -> print res >> let instructions = (astToInstructions res) in showInstructions instructions >> writeCompiledInstructionsToFile "output" (compileInstructions instructions)

-- | Main
main :: IO ()
main =  putStrLn "Start of Interpretation Lisp" >> inputLoop []
