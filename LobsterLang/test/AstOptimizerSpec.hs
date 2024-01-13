{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- AstOptimizerSpec
-}

module AstOptimizerSpec where

import Test.Hspec
import AST
import AstOptimizer
import Scope

spec :: Spec
spec = do
    describe "Value Ast optimization tests" $ do
        it "Basic Value" $ do
            optimizeAst [] [Value 5] False `shouldBe` [Right (Result (Value 5))]
        it "Basic Value (multiple)" $ do
            optimizeAst [] [Value 5, Value 8] False `shouldBe` [Right (Result (Value 5)), Right (Result (Value 8))]
    describe "Boolean Ast optimization tests" $ do
        it "Basic Boolean" $ do
            optimizeAst [] [Boolean True] False `shouldBe` [Right (Result (Boolean True))]
        it "Basic Boolean (multiple)" $ do
            optimizeAst [] [Boolean True, Boolean False] False `shouldBe` [Right (Result (Boolean True)), Right (Result (Boolean False))]
    describe "String Ast optimization tests" $ do
        it "Basic String" $ do
            optimizeAst [] [String "blegh"] False `shouldBe` [Right (Result (String "blegh"))]
        it "Basic String (multiple)" $ do
            optimizeAst [] [String "blegh", String "aaaaa"] False `shouldBe` [Right (Result (String "blegh")), Right (Result (String "aaaaa"))]
    describe "List Ast optimization tests" $ do
        it "Empty list" $ do
            optimizeAst [] [List []] False `shouldBe` [Right (Result (List []))]
        it "Optimizable list" $ do
            optimizeAst [] [List [Call "+" [Value 1, Value 2], Value 5]] False `shouldBe` [Right (Result (List [Value 3, Value 5]))]
        it "Unoptimizable list" $ do
            optimizeAst [] [List [Value 5, Boolean True, Value 9, String "vzb"]] False `shouldBe` [Right (Result (List [Value 5, Boolean True, Value 9, String "vzb"]))]
        it "Unoptimizable list 2" $ do
            optimizeAst [Variable "a" (Value 5) 0] [List [Value 5, Symbol "a" Nothing]] False `shouldBe` [Right (Result (List [Value 5, Symbol "a" Nothing]))]
        it "Unoptimizable list error" $ do
            optimizeAst [] [List [Value 5, Symbol "a" Nothing]] False `shouldBe` [Left (Error "Symbol 'a' doesn't exist in the current or global scope" (Symbol "a" Nothing))]
    describe "Define Ast optimization tests" $ do
        it "Unoptimizable Define" $ do
            optimizeAst [] [Define "a" (Value 5)] False `shouldBe` [Right (Result (Define "a" (Value 5)))]
        it "Optimizable Define" $ do
            optimizeAst [] [Define "a" (Call "+" [Value 5, Value 5])] False `shouldBe` [Right (Result (Define "a" (Value 10)))]
        it "Error Define" $ do
            optimizeAst [] [Define "a" (Call "+" [Value 5])] False `shouldBe` [Left (Error "Not enough parameters for binary operator '+'" (Call "+" [Value 5]))]
        it "Error Define 2" $ do
            optimizeAst [] [Define "a" (Define "b" (Value 2))] False `shouldBe` [Left (Error "Cannot define with no value" (Define "a" (Define "b" (Value 2))))]
        it "Error Define 3" $ do
            optimizeAst [] [Define "a" (Symbol "b" Nothing)] False `shouldBe` [Left (Error "Symbol 'b' doesn't exist in the current or global scope" (Symbol "b" Nothing))]
        it "Define from symbol in function" $ do
            optimizeAst [] [Define "a" (Symbol "b" Nothing)] True `shouldBe` [Right (Result (Define "a" (Symbol "b" Nothing)))]
    describe "Symbol Ast optimization tests" $ do
        it "Simple symbol (in function)" $ do
            optimizeAst [] [Symbol "a" Nothing] True `shouldBe` [Right (Result (Symbol "a" Nothing))]
        it "Simple symbol (out of function)" $ do
            optimizeAst [Variable "a" (Value 5) 0] [Symbol "a" Nothing] False `shouldBe` [Right (Result (Symbol "a" Nothing))]
        it "Error doesn't exist symbol (out of function)" $ do
            optimizeAst [] [Symbol "a" Nothing] False `shouldBe` [Left (Error "Symbol 'a' doesn't exist in the current or global scope" (Symbol "a" Nothing))]
        it "Error function symbol (in function) with unoptimizable params (doesn't exist)" $ do
            optimizeAst [] [Symbol "a" (Just [Value 5])] True `shouldBe` [Right (Result (Symbol "a" (Just [Value 5])))]
        it "Error function symbol (out of function) with unoptimizable params" $ do
            optimizeAst [] [Symbol "a" (Just [Value 5])] False `shouldBe` [Left (Error "Symbol 'a' doesn't exist in the current or global scope" (Symbol "a" (Just [Value 5])))]
        it "Simple function symbol (out of function) with unoptimizable params" $ do
            optimizeAst [Variable "a" (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 1]) Nothing) 0] [Symbol "a" (Just [Value 5])] False `shouldBe` [Right (Result (Symbol "a" (Just [Value 5])))]
        it "Error function symbol (out of function) with unoptimizable params" $ do
            optimizeAst [Variable "a" (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 1]) Nothing) 0] [Symbol "a" (Just [Value 5, Value 6])] False `shouldBe` [Left (Error "Expression takes 1 parameters, got 2" (Symbol "a" (Just [Value 5, Value 6])))]
        it "Simple function symbol (out of function) with optimizable params" $ do
            optimizeAst [Variable "a" (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 1]) Nothing) 0] [Symbol "a" (Just [Call "+" [Value 1, Value 5]])] False `shouldBe` [Right (Result (Symbol "a" (Just [Value 6])))]
        it "Error function symbol (out of function) with optimizable params" $ do
            optimizeAst [Variable "a" (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 1]) Nothing) 0] [Symbol "a" (Just [Call "+" [Value 1, Boolean True]])] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [Value 1, Boolean True]))]
    describe "Operator Ast optimization tests" $ do
        it "Optimize +" $ do
            optimizeAst [] [Call "+" [Value 5, Value 8]] False `shouldBe` [Right (Result (Value 13))]
        it "Optimize -" $ do
            optimizeAst [] [Call "-" [Value 5, Value 8]] False `shouldBe` [Right (Result (Value (-3)))]
        it "Optimize &&" $ do
            optimizeAst [] [Call "&&" [Boolean True, Boolean False]] False `shouldBe` [Right (Result (Boolean False))]
        it "Optimize !" $ do
            optimizeAst [] [Call "!" [Boolean True]] False `shouldBe` [Right (Result (Boolean False))]
        it "Optimize @" $ do
            optimizeAst [] [Call "@" [Value 56]] False `shouldBe` [Right (Result (String "56"))]
        it "Optimize nested operators" $ do
            optimizeAst [] [Call "*" [Call "+" [Value 8, Value 2], Call "-" [Value 9, Call "%" [Value 126, Value 10]]]] False `shouldBe` [Right (Result (Value 30))]
        it "Optimize + with symbol" $ do
            optimizeAst [Variable "a" (Value 5) 0] [Call "+" [Symbol "a" Nothing, Value 8]] False `shouldBe` [Right (Result (Call "+" [Symbol "a" Nothing, Value 8]))]
        it "Error not Value" $ do
            optimizeAst [] [Call "+" [List [Value 8, Value 9], Value 8]] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [List [Value 8, Value 9], Value 8]))]
        it "Error symbol doesn't exist" $ do
            optimizeAst [] [Call "+" [Symbol "a" Nothing, Value 8]] False `shouldBe` [Left (Error "Symbol 'a' doesn't exist in the current or global scope" (Call "+" [Symbol "a" Nothing, Value 8]))]
    describe "Cond Ast Optimizations" $ do
        it "Optimize condition in Cond" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Call "||" [Boolean True, Boolean False], Symbol "a" Nothing]) (Value 1) Nothing] False `shouldBe` [Right (Result (Cond (Call "&&" [Boolean True, Symbol "a" Nothing]) (Value 1) Nothing))]
        it "Optimize true ast in Cond" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Call "+" [Value 5, Value 8]) Nothing] False `shouldBe` [Right (Result (Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 13) Nothing))]
        it "Optimize false ast in Cond" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 1) (Just (Call "+" [Value 5, Value 8]))] False `shouldBe` [Right (Result (Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 1) (Just (Value 13))))]
        it "Optimize condition in Cond error" $ do
            optimizeAst [] [Cond (Call "&&" [Symbol "a" Nothing, Call "||" [Boolean True, Boolean False]]) (Value 1) Nothing] False `shouldBe` [Left (Error "Symbol 'a' doesn't exist in the current or global scope" (Symbol "a" Nothing))]
        it "Optimize true ast in Cond error" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Call "+" [String "bleg", Value 8]) Nothing] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [String "bleg", Value 8]))]
        it "Optimize false ast in Cond error" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 1) (Just (Call "+" [Value 5, List [String "bleg"]]))] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [Value 5, List [String "bleg"]]))]
        it "Optimize condition in Cond warning" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Cond (Boolean True) (Symbol "a" Nothing) Nothing) (Value 1) Nothing] False `shouldBe` [Right (Result (Cond (Symbol "a" Nothing) (Value 1) Nothing))]
        it "Optimize true ast in Cond warning" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Cond (Boolean True) (Symbol "a" Nothing) Nothing) Nothing] False `shouldBe` [Right (Result (Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Symbol "a" Nothing) Nothing))]
        it "Optimize false ast in Cond warning" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 1) (Just (Cond (Boolean True) (Symbol "a" Nothing) Nothing))] False `shouldBe` [Right (Result (Cond (Call "&&" [Symbol "a" Nothing, Boolean True]) (Value 1) (Just (Symbol "a" Nothing))))]
        it "Optimize always true" $ do
            optimizeAst [] [Cond (Boolean True) (Value 1) (Just (Value 8))] False `shouldBe` [Right (Warning "Condition is always true" (Value 1))]
        it "Optimize always false" $ do
            optimizeAst [] [Cond (Boolean False) (Value 1) (Just (Value 8))] False `shouldBe` [Right (Warning "Condition is always false" (Value 8))]
        it "Optimize always false (no else)" $ do
            optimizeAst [] [Cond (Boolean False) (Value 1) Nothing] False `shouldBe` [Right (Warning "Condition is always false" (Cond (Boolean False) (Value 1) Nothing))]
        it "Unoptimizable Cond" $ do
            optimizeAst [Variable "a" (Boolean True) 0] [Cond (Call "&&" [Boolean True, Symbol "a" Nothing]) (Value 1) Nothing] False `shouldBe` [Right (Result (Cond (Call "&&" [Boolean True, Symbol "a" Nothing]) (Value 1) Nothing))]
    describe "FunctionValue Ast optimization tests" $ do
        -- without params
        it "Optimize inner ast in FunctionValue error" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "$" [Define "a" (Call "+" [Boolean True, Value 1]), Call "+" [Symbol "a" Nothing, Value 1]]) Nothing] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [Boolean True, Value 1]))]
        it "Optimize inner ast in FunctionValue" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Call "+" [Value 1, Value 1]]) Nothing] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) Nothing))]
        it "Optimize inner ast in FunctionValue warning" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Cond (Boolean True) (Value 2) Nothing]) Nothing] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) Nothing))]
        -- with params (inner)
        it "Optimize inner ast in FunctionValue with params" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Call "+" [Value 1, Value 1]]) (Just [Value 2])] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Value 2])))]
        it "Optimize inner ast in FunctionValue with params warning" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Cond (Boolean True) (Value 2) Nothing]) (Just [Value 2])] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Value 2])))]
        it "Optimize inner ast in FunctionValue with params error" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Call "+" [Boolean True, Value 1]]) (Just [Value 2])] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [Boolean True, Value 1]))]
        -- with params (params)
        it "Empty params" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [])] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) Nothing))]
        it "Unoptimizable params error" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Call "+" [Value 1, Boolean True]])] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (Call "+" [Value 1, Boolean True]))]
        it "Optimizable params" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Call "+" [Value 1, Value 1]])] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Value 2])))]
        it "Unoptimizable params" $ do
            optimizeAst [] [FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Value 2])] False `shouldBe` [Right (Result (FunctionValue ["x"] (Call "+" [Symbol "x" Nothing, Value 2]) (Just [Value 2])))]
        -- currying
        it "Currying" $ do
            optimizeAst [] [FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Value 2])] False `shouldBe` [Right (Result (FunctionValue ["b"] (Call "$" [Define "a" (Value 2), Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]]) Nothing))]
        -- Check valaidity of func
        it "Unoptimizable func with params" $ do
            optimizeAst [] [FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Value 2, Value 3])] False `shouldBe` [Right (Result (FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Value 2, Value 3])))]
        it "Unoptimizable func with params (symbol don't exist in func)" $ do
            optimizeAst [] [FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Symbol "c" Nothing, Value 3])] True `shouldBe` [Right (Result (FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Symbol "c" Nothing, Value 3])))]
        it "Unoptimizable func with params (symbol don't exist out of func)" $ do
            optimizeAst [] [FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Symbol "c" Nothing, Value 3])] False `shouldBe` [Left (Error "Symbol 'c' doesn't exist in the current or global scope" (FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Symbol "c" Nothing, Value 3])))]
        it "Unoptimizable func with params error" $ do
            optimizeAst [] [FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Value 2, Boolean True])] False `shouldBe` [Left (Error "One or more parameters of binary operator '+' is invalid" (FunctionValue ["a", "b"] (Call "+" [Symbol "a" Nothing, Symbol "b" Nothing]) (Just [Value 2, Boolean True])))]
    describe "Advanced Ast optimization tests" $ do
        it "Call then symbol" $ do
            optimizeAst [Variable "a" (Value 5) 0] [Call "-" [Value 5, Value 8], Call "+" [Symbol "a" Nothing, Value 8]] False `shouldBe` [Right (Result (Value (-3))), Right (Result (Call "+" [Symbol "a" Nothing, Value 8]))]
        it "Define then call" $ do
            optimizeAst [] [Define "a" (Value 5), Call "-" [Symbol "a" Nothing, Value 8]] False `shouldBe` [Right (Result (Define "a" (Value 5))), Right (Result (Call "-" [Symbol "a" Nothing, Value 8]))]
        it "Infinite recursion" $ do
            optimizeAst [Variable "eh" (FunctionValue ["x"] (Symbol "eh" (Just [Symbol "x" Nothing])) Nothing) 0] [Symbol "eh" (Just [AST.Value 1])] False `shouldBe` [Right (Warning "Possible infinite recursion" (Symbol "eh" (Just [AST.Value 1])))]
        it "Infinite recursion in define" $ do
            optimizeAst [Variable "eh" (FunctionValue ["x"] (Symbol "eh" (Just [Symbol "x" Nothing])) Nothing) 0] [Define "a" (Symbol "eh" (Just [AST.Value 1]))] False `shouldBe` [Right (Warning "Possible infinite recursion" (Define "a" (Symbol "eh" (Just [AST.Value 1]))))]
