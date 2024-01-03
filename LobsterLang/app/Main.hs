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
