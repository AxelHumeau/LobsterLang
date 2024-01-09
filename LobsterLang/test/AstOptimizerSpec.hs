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
            optimizeAst [] [Define "a" (Call "+" [Value 5])] False `shouldBe` [Left (Error "Not enough parameter for binary operator '+'" (Call "+" [Value 5]))]
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
    describe "Advanced Ast optimization tests" $ do
        it "Call then symbol" $ do
            optimizeAst [Variable "a" (Value 5) 0] [Call "-" [Value 5, Value 8], Call "+" [Symbol "a" Nothing, Value 8]] False `shouldBe` [Right (Result (Value (-3))), Right (Result (Call "+" [Symbol "a" Nothing, Value 8]))]
