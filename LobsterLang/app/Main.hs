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

-- | Infinite loop until EOF from the user
inputLoop :: [Scope.ScopeMb] -> IO ()
inputLoop stack = isEOF >>= \end -> if end then print "End of Interpretation GLaDOS" else
    getLine >>= \line -> case parseLisp line stack of
        (res, stack') -> print res >> inputLoop stack'

-- | Main
main :: IO ()
main =  print "Start of Interpretation Lisp" >> inputLoop []
