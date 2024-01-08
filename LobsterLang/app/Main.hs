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
import System.Environment (getArgs)
import Control.Exception
import SExpr (SExpr)
-- import Compiler

-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
-- inputLoop = print
inputLoop stack = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case runParser parseLisp (0, 0) line of
        Left err -> print err >> exitWith (ExitFailure 84)
        Right (res, _, _) -> interpretateInfo res stack

interpretateInfo :: [SExpr] -> [Scope.ScopeMb] -> IO ()
interpretateInfo [] _ = putStr ""
interpretateInfo (x:xs) stack = case interpretateLisp x stack of
    Left err -> print err
    Right (res, stack') -> case res of
        Nothing -> interpretateInfo xs stack'
        Just value -> print value >> print stack' >> interpretateInfo xs stack'

compileFile :: String -> IO ()
compileFile s = case runParser parseLisp (0, 0) s of
        Left err -> print err >> exitWith (ExitFailure 84)
        Right (res, _, _) -> interpretateInfo res []
        -- (Right (Just res), stack') -> let instructions = (astToInstructions (AST.Cond (Boolean True) (Value 1) (Just (AST.Call "CallHere" [(Value 0)])))) in showInstructions instructions >> writeCompiledInstructionsToFile "output" (compileInstructions instructions)


checkArgs :: [String] -> IO ()
checkArgs ("-i": _) = print "Launch Interpreter" >> inputLoop []
checkArgs (file:_) = either
                        (\_ -> print "File doesn't exist" >> exitWith (ExitFailure 84))
                        compileFile
                    =<< (try (readFile file) :: IO (Either SomeException String))
checkArgs _ = exitWith (ExitFailure 84)

-- | Main
main :: IO ()
main = getArgs >>= \argv -> checkArgs argv
-- inputLoop new = isEOF >>= \end -> if end then putStrLn "End of Interpretation GLaDOS" else
--     getLine >>= \line -> case parseLisp line new of
--         (Left err, _) -> putStrLn ("\ESC[31m\ESC[1mThe lobster is angry: " ++ err ++ "\ESC[0m") >> inputLoop new
--         (Right Nothing, stack) -> inputLoop stack
--         (Right (Just res), stack') -> print res >> inputLoop stack'

-- -- | Main
-- main :: IO ()
-- main =  putStrLn "Start of Interpretation Lisp" >> inputLoop []
