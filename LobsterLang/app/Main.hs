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
import qualified AstEval
import qualified AstOptimizer
import qualified Compiler
import qualified Vm
import qualified CompiletoVm
import Control.Exception
import qualified AST
import AstOptimizer (optimizeAst)

import Debug.Trace

lobsterNotHappy :: String -> String -> String -> String
lobsterNotHappy color state str = "\ESC[" ++ color ++ "m\ESC[1mThe lobster is " ++ state ++ ": " ++ str ++ "\ESC[0m"

-- | Return a Result that contain the evaluation of our Lisp String
-- Takes as parameter the string that need to be evaluated and the Stack (Environment)
interpretateLobster :: AST.Ast -> [Scope.ScopeMb] -> Either String (Maybe AST.Ast, [Scope.ScopeMb])
interpretateLobster value stack = case AstEval.evalAst stack value of
        (Left err, _) -> Left err
        (Right res', stack') -> Right (res', stack')

-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
-- inputLoop = print
inputLoop stack = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case runParser parseLobster (0, 0) line of
        Left err -> putStrLn (lobsterNotHappy "34" "angry" err) >> inputLoop stack
        Right (res, [], _) -> interpretateInfo res stack
        Right (_, _, pos) -> putStrLn (lobsterNotHappy "31" "angry" (errorParsing pos)) >> inputLoop stack

interpretateInfo :: [AST.Ast] -> [Scope.ScopeMb] -> IO ()
interpretateInfo [] stack = inputLoop stack
interpretateInfo (x:xs) stack = case interpretateLobster x stack of
    Left err -> putStrLn (lobsterNotHappy "31" "angry" err) >> inputLoop stack
    Right (res, stack') -> case res of
        Nothing -> interpretateInfo xs stack'
        Just value -> print value >> interpretateInfo xs stack'

checkCompileInfo :: [Either AstOptimizer.AstError AstOptimizer.AstOptimised] -> [Either AstOptimizer.AstError AstOptimizer.AstOptimised] -> IO [Either AstOptimizer.AstError AstOptimizer.AstOptimised]
checkCompileInfo [] list = return list
checkCompileInfo (x:xs) list = case x of
    Left (AstOptimizer.Error err ast) -> putStrLn (lobsterNotHappy "31" "angry" (err ++ " caused by: " ++ show ast)) >> checkCompileInfo xs (list ++ [x])
    Right (AstOptimizer.Result _) -> checkCompileInfo xs (list ++ [x])
    Right (AstOptimizer.Warning warning ast) -> putStrLn (lobsterNotHappy "33" "worried" (warning ++ " optimize to" ++ show ast)) >> checkCompileInfo xs (list ++ [x])

compileInfo :: String -> [AST.Ast] -> [Scope.ScopeMb] -> IO ()
compileInfo _ [] _ = putStr ""
compileInfo filename list stack = checkCompileInfo (optimizeAst stack list False) [] >>= \res -> case sequence res of
        Left _ -> exitWith (ExitFailure 84)
        Right value -> Compiler.compile (map AstOptimizer.fromOptimised value) (filename ++ "o") True

compileFile :: String -> String -> IO ()
compileFile file s = case runParser parseLobster (0, 0) s of
        Left err -> print err >> exitWith (ExitFailure 84)
        Right (res, [], _) -> compileInfo file res []
        Right (_, _, pos) -> putStrLn (lobsterNotHappy "34" "angry" (errorParsing pos))

checkArgs :: [String] -> IO ()
checkArgs [] = print "Launch Interpreter" >> inputLoop []
checkArgs ("-e":file:_) = CompiletoVm.makeConvert file
                        >>= \instructions -> trace ("instructions to execute" ++ show instructions) print (Vm.exec 0 [] [] instructions [])
checkArgs (file:_) = either
                        (\_ -> print "File doesn't exist" >> exitWith (ExitFailure 84))
                        (compileFile file)
                        =<< (try (readFile file) :: IO (Either SomeException String))

-- | Main
main :: IO ()
main = getArgs >>= \argv -> checkArgs argv
