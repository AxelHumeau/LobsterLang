{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- AstEvalTests
-}

module AstEvalSpec where

import Test.Hspec
import AstEval
import AST
import Scope

spec :: Spec
spec = do
    describe "Ast evaluation tests" $ do
        -- Basic evaluation
        it "Check Value" $ do
            evalAst [] (AST.Value 5) `shouldBe` (Just (AST.Value 5), [])
        it "Check Boolean True" $ do
            evalAst [] (AST.Boolean True) `shouldBe` (Just (AST.Boolean True), [])
        it "Check Boolean False" $ do
            evalAst [] (AST.Boolean False) `shouldBe` (Just (AST.Boolean False), [])
        -- Value operators
        it "Check valid operation +" $ do
            evalAst [] (Call "+" [AST.Value 5, AST.Value 8]) `shouldBe` (Just (AST.Value 13), [])
        it "Check valid operation -" $ do
            evalAst [] (Call "-" [AST.Value 5, AST.Value 8]) `shouldBe` (Just (AST.Value (-3)), [])
        it "Check valid operation *" $ do
            evalAst [] (Call "*" [AST.Value 5, AST.Value 8]) `shouldBe` (Just (AST.Value 40), [])
        it "Check valid operation /" $ do
            evalAst [] (Call "/" [AST.Value 15, AST.Value 5]) `shouldBe` (Just (AST.Value 3), [])
        it "Check invalid operation / (division by zero)" $ do
            evalAst [] (Call "/" [AST.Value 15, AST.Value 0]) `shouldBe` (Nothing, [])
        it "Check valid operation %" $ do
            evalAst [] (Call "%" [AST.Value 15, AST.Value 4]) `shouldBe` (Just (AST.Value 3), [])
        it "Check invalid operation % (division by zero)" $ do
            evalAst [] (Call "%" [AST.Value 15, AST.Value 0]) `shouldBe` (Nothing, [])
        it "Check invalid value binary operation (wrong type)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Boolean True, AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value binary operation (wrong type 2)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8, AST.Boolean False]) `shouldBe` (Nothing, [])
        it "Check invalid value binary operation (not enough ast parameters)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value binary operation (too much ast parameters)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Nothing, [])
        -- Value comparison operators
        it "Check valid operation ==" $ do
            evalAst [] (Call "==" [AST.Value 5, AST.Value 5]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation !=" $ do
            evalAst [] (Call "!=" [AST.Value 5, AST.Value 8]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation <" $ do
            evalAst [] (Call "<" [AST.Value 8, AST.Value 2]) `shouldBe` (Just (AST.Boolean False), [])
        it "Check valid operation <=" $ do
            evalAst [] (Call "<=" [AST.Value 15, AST.Value 15]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation >" $ do
            evalAst [] (Call ">" [AST.Value 8, AST.Value 2]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation >=" $ do
            evalAst [] (Call ">=" [AST.Value 15, AST.Value 15]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check invalid value comparison binary operation (wrong type)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Boolean True, AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (wrong type 2)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8, AST.Boolean False]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (not enough ast parameters)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Nothing, [])
        -- Boolean operators
        it "Check valid operation &&" $ do
            evalAst [] (Call "&&" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Just (AST.Boolean False), [])
        it "Check valid operation && 2" $ do
            evalAst [] (Call "&&" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Just (AST.Boolean False), [])
        it "Check valid operation ||" $ do
            evalAst [] (Call "||" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation || 2" $ do
            evalAst [] (Call "||" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation ^^ (xor)" $ do
            evalAst [] (Call "^^" [AST.Boolean True, AST.Boolean True]) `shouldBe` (Just (AST.Boolean False), [])
        it "Check valid operation ^^ (xor) 2" $ do
            evalAst [] (Call "^^" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation ^^ (xor) 3" $ do
            evalAst [] (Call "^^" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation ! (not)" $ do
            evalAst [] (Call "!" [AST.Boolean False]) `shouldBe` (Just (AST.Boolean True), [])
        it "Check valid operation ! (not) 2" $ do
            evalAst [] (Call "!" [AST.Boolean True]) `shouldBe` (Just (AST.Boolean False), [])
        it "Check invalid operation ! (not)" $ do
            evalAst [] (Call "!" [AST.Value 5]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (wrong type)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Boolean True, AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (wrong type 2)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Boolean False]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (not enough ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8]) `shouldBe` (Nothing, [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Nothing, [])
        -- Check Define
        it "Check unknonw variable" $ do
            evalAst (beginScope []) (AST.Symbol "bar") `shouldBe` (Nothing, [ScopeBegin])
        it "Check variable definition" $ do
            evalAst (beginScope []) (Define "foo" (AST.Value 1)) `shouldBe` (Nothing, [Variable "foo" (AST.Value 1), ScopeBegin])
        it "Check variable definition 2" $ do
            evalAst (beginScope []) (Define "bar" (Call "+" [AST.Value 1, AST.Value 5])) `shouldBe` (Nothing, [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin])
        it "Check variable usage" $ do
            evalAst [Variable "foo" (AST.Value 1), ScopeBegin] (AST.Symbol "foo") `shouldBe` (Just (AST.Value 1), [Variable "foo" (AST.Value 1), ScopeBegin])
        it "Check variable usage 2" $ do
            evalAst [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin] (AST.Symbol "bar") `shouldBe` (Just (AST.Value 6), [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin])
        it "Check invalid function usage" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Boolean True]), ScopeBegin] (Call "foo" [AST.Value 5]) `shouldBe` (Nothing, [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Boolean True]), ScopeBegin])
        it "Check basic function definition" $ do
            evalAst (beginScope []) (Define "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]) Nothing)) `shouldBe` (Nothing, [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check basic function usage" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin] (Call "foo" [AST.Value 5]) `shouldBe` (Just (AST.Value 6), [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check multi-parameters function definition" $ do
            evalAst (beginScope []) (Define "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]) Nothing)) `shouldBe` (Nothing, [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin])
        it "Check multi-parameters function usage" $ do
            evalAst [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin] (Call "3+" [AST.Value 5, AST.Value 6, AST.Value (-9)]) `shouldBe` (Just (AST.Value 2), [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin])
        -- Check Lambda usage
        it "Check +1 lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]) (Just [AST.Value 5])) `shouldBe` (Just (AST.Value 6), [])
        it "Check square lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) (Just [AST.Value 5])) `shouldBe` (Just (AST.Value 25), [])
        it "Check lambda not used" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) Nothing) `shouldBe` (Nothing, [])
        -- Check Cond
        it "Check true Cond" $ do
            evalAst [] (Cond (AST.Boolean True) (AST.Value 5) Nothing) `shouldBe` (Just (AST.Value 5), [])
        it "Check false Cond" $ do
            evalAst [] (Cond (AST.Boolean False) (AST.Value 5) Nothing) `shouldBe` (Nothing, [])
        it "Check true Cond else" $ do
            evalAst [] (Cond (AST.Boolean True) (AST.Value 5) (Just (AST.Value 6))) `shouldBe` (Just (AST.Value 5), [])
        it "Check false Cond else" $ do
            evalAst [] (Cond (AST.Boolean False) (AST.Value 5) (Just (AST.Value 6))) `shouldBe` (Just (AST.Value 6), [])
        it "Check invalid condition" $ do
            evalAst [] (Cond (Define "a" (AST.Value 5)) (AST.Value 6) Nothing) `shouldBe` (Nothing, [])
        it "Check invalid condition 2" $ do
            evalAst [] (Cond (AST.Value 5) (AST.Value 6) Nothing) `shouldBe` (Nothing, [])
        -- Advanced tests
        it "Check factorial definition" $ do
            evalAst [] (Define "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]]))) Nothing)) `shouldBe` (Nothing, [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))])
        it "Check factorial usage" $ do
            evalAst [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))] (Call "fact" [AST.Value 6]) `shouldBe` (Just (AST.Value 720), [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))])
