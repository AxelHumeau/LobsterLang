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
import qualified AST

-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
-- inputLoop = print
inputLoop stack = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case runParser parseLobster (0, 0) line of
        Left err -> putStrLn ("\ESC[34m\ESC[1mThe lobster is angry: " ++ err ++ "\ESC[0m") >> inputLoop stack
        Right (res, _, _) -> interpretateInfo res stack

interpretateInfo :: [AST.Ast] -> [Scope.ScopeMb] -> IO ()
interpretateInfo [] stack = inputLoop stack
interpretateInfo (x:xs) stack = case interpretateLisp x stack of
    Left err -> putStrLn ("\ESC[31m\ESC[1mThe lobster is angry: " ++ err ++ "\ESC[0m") >> inputLoop stack
    Right (res, stack') -> case res of
        Nothing -> interpretateInfo xs stack'
        Just value -> print value >> interpretateInfo xs stack'

compileInfo :: [AST.Ast] -> [Scope.ScopeMb] -> IO ()
compileInfo [] _ = putStr ""
compileInfo (x:xs) stack = case interpretateLisp x stack of
    Left err -> putStrLn ("\ESC[31m\ESC[1mThe lobster is angry: " ++ err ++ "\ESC[0m") >> exitWith (ExitFailure 84)
    Right (res, stack') -> case res of
        Nothing -> compileInfo xs stack'
        Just value -> print value >> compileInfo xs stack'

compileFile :: String -> IO ()
compileFile s = case runParser parseLobster (0, 0) s of
        Left err -> print err >> exitWith (ExitFailure 84)
        Right (res, [], _) -> compileInfo res []
        Right (_, _, (row, col)) -> print ("Error on parsing on '" ++ show row ++ "' '" ++ show col)

checkArgs :: [String] -> IO ()
checkArgs [] = print "Launch Interpreter" >> inputLoop []
checkArgs (file:_) = either
                        (\_ -> print "File doesn't exist" >> exitWith (ExitFailure 84))
                        compileFile
                    =<< (try (readFile file) :: IO (Either SomeException String))

-- | Main
main :: IO ()
main = getArgs >>= \argv -> checkArgs argv
