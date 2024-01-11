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
    describe "Basic Ast evaluation tests" $ do
        -- Basic evaluation
        it "Check Value" $ do
            evalAst [] (AST.Value 5) `shouldBe` (Right (Just (AST.Value 5)), [])
        it "Check Boolean True" $ do
            evalAst [] (AST.Boolean True) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check Boolean False" $ do
            evalAst [] (AST.Boolean False) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check String" $ do
            evalAst [] (AST.String "Blegh") `shouldBe` (Right (Just (AST.String "Blegh")), [])
        it "Check String conversion String" $ do
            evalAst [] (Call "@" [AST.String "Blegh"]) `shouldBe` (Right (Just (AST.String "Blegh")), [])
        it "Check String conversion Value" $ do
            evalAst [] (Call "@" [AST.Value 8]) `shouldBe` (Right (Just (AST.String "8")), [])
        it "Check String conversion Boolean" $ do
            evalAst [Variable "x" (AST.Value 5) 0] (Call "@" [AST.Symbol "x" Nothing]) `shouldBe` (Right (Just (AST.String "5")), [Variable "x" (AST.Value 5) 0])
        it "Check String conversion Symbol" $ do
            evalAst [] (Call "@" [AST.Boolean True]) `shouldBe` (Right (Just (AST.String "True")), [])
        it "Check String conversion Lambda" $ do
            evalAst [] (Call "@" [AST.FunctionValue ["x"] (AST.Symbol "x" Nothing) Nothing]) `shouldBe` (Left "Cannot convert lambda to string", [])
        it "Check invalid String conversion" $ do
            evalAst [] (Call "@" []) `shouldBe` (Left "Not enough parameters for string conversion", [])
        it "Check invalid String conversion 2" $ do
            evalAst [] (Call "@" [AST.Value 8, AST.Value 8]) `shouldBe` (Left "Too much parameters for string conversion", [])
        it "Check invalid String conversion 3" $ do
            evalAst [] (Call "@" [Define "a" (AST.Value 5)]) `shouldBe` (Left "Cannot convert no evaluation to string", [])
        it "Check invalid String conversion 4" $ do
            evalAst [] (Call "@" [Call "+" [AST.Value 5, AST.Boolean True]]) `shouldBe` (Left "One or more parameters of binary operator '+' is invalid", [])
    describe "Value Ast evaluation tests" $ do
        -- Value operators
        it "Check valid operation +" $ do
            evalAst [] (Call "+" [AST.Value 5, AST.Value 8]) `shouldBe` (Right (Just (AST.Value 13)), [])
        it "Check valid operation -" $ do
            evalAst [] (Call "-" [AST.Value 5, AST.Value 8]) `shouldBe` (Right (Just (AST.Value (-3))), [])
        it "Check valid operation *" $ do
            evalAst [] (Call "*" [AST.Value 5, AST.Value 8]) `shouldBe` (Right (Just (AST.Value 40)), [])
        it "Check valid operation /" $ do
            evalAst [] (Call "/" [AST.Value 15, AST.Value 5]) `shouldBe` (Right (Just (AST.Value 3)), [])
        it "Check invalid operation / (division by zero)" $ do
            evalAst [] (Call "/" [AST.Value 15, AST.Value 0]) `shouldBe` (Left "Cannot divide by zero", [])
        it "Check valid operation %" $ do
            evalAst [] (Call "%" [AST.Value 15, AST.Value 4]) `shouldBe` (Right (Just (AST.Value 3)), [])
        it "Check invalid operation % (division by zero)" $ do
            evalAst [] (Call "%" [AST.Value 15, AST.Value 0]) `shouldBe` (Left "Cannot divide by zero", [])
        it "Check invalid value binary operation (wrong type)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Boolean True, AST.Value 8]) `shouldBe` (Left "One or more parameters of binary operator '+' is invalid", [])
        it "Check invalid value binary operation (wrong type 2)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8, AST.Boolean False]) `shouldBe` (Left "One or more parameters of binary operator '+' is invalid", [])
        it "Check invalid value binary operation (not enough ast parameters)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8]) `shouldBe` (Left "Not enough parameters for binary operator '+'", [])
        it "Check invalid value binary operation (too much ast parameters)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameters for binary operator '+'", [])
    describe "Value comparison evaluation tests" $ do
        -- Value comparison operators
        it "Check valid operation ==" $ do
            evalAst [] (Call "==" [AST.Value 5, AST.Value 5]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation !=" $ do
            evalAst [] (Call "!=" [AST.Value 5, AST.Value 8]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation <" $ do
            evalAst [] (Call "<" [AST.Value 8, AST.Value 2]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check valid operation <=" $ do
            evalAst [] (Call "<=" [AST.Value 15, AST.Value 15]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation >" $ do
            evalAst [] (Call ">" [AST.Value 8, AST.Value 2]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation >=" $ do
            evalAst [] (Call ">=" [AST.Value 15, AST.Value 15]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check invalid value comparison binary operation (wrong type)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Boolean True, AST.Value 8]) `shouldBe` (Left "One or more parameters of binary operator '==' is invalid", [])
        it "Check invalid value comparison binary operation (wrong type 2)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8, AST.Boolean False]) `shouldBe` (Left "One or more parameters of binary operator '==' is invalid", [])
        it "Check invalid value comparison binary operation (not enough ast parameters)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8]) `shouldBe` (Left "Not enough parameters for binary operator '=='", [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameters for binary operator '=='", [])
    describe "Boolean operators evaluation tests" $ do
        -- Boolean operators
        it "Check valid operation &&" $ do
            evalAst [] (Call "&&" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check valid operation && 2" $ do
            evalAst [] (Call "&&" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check valid operation ||" $ do
            evalAst [] (Call "||" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation || 2" $ do
            evalAst [] (Call "||" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation ^^ (xor)" $ do
            evalAst [] (Call "^^" [AST.Boolean True, AST.Boolean True]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check valid operation ^^ (xor) 2" $ do
            evalAst [] (Call "^^" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation ^^ (xor) 3" $ do
            evalAst [] (Call "^^" [AST.Boolean False, AST.Boolean True]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation ! (not)" $ do
            evalAst [] (Call "!" [AST.Boolean False]) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check valid operation ! (not) 2" $ do
            evalAst [] (Call "!" [AST.Boolean True]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check valid operation ! (not) with symbol" $ do
            evalAst [Variable "a" (Boolean True) 0] (Call "!" [AST.Symbol "a" Nothing]) `shouldBe` (Right (Just (AST.Boolean False)), [Variable "a" (Boolean True) 0])
        it "Check valid operation ! (not) with eval" $ do
            evalAst [] (Call "!" [Call "&&" [AST.Boolean True, AST.Boolean True]]) `shouldBe` (Right (Just (AST.Boolean False)), [])
        it "Check invalid operation ! (not)" $ do
            evalAst [] (Call "!" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Left "Invalid number of parameter for unary operator '!'", [])
        it "Check invalid operation ! (not) 2" $ do
            evalAst [] (Call "!" [AST.Value 5]) `shouldBe` (Left "Parameter of unary operator '!' isn't a boolean", [])
        it "Check invalid operation ! (not) with symbol" $ do
            evalAst [Variable "a" (Value 8) 0] (Call "!" [Symbol "a" Nothing]) `shouldBe` (Left "Parameter of unary operator '!' isn't a boolean", [Variable "a" (Value 8) 0])
        it "Check invalid value comparison binary operation (wrong type)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Boolean True, AST.Value 8]) `shouldBe` (Left "One or more parameters of binary operator '&&' is invalid", [])
        it "Check invalid value comparison binary operation (wrong type 2)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Boolean False]) `shouldBe` (Left "One or more parameters of binary operator '&&' is invalid", [])
        it "Check invalid value comparison binary operation (not enough ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8]) `shouldBe` (Left "Not enough parameters for binary operator '&&'", [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameters for binary operator '&&'", [])
    describe "Define and function evaluation tests" $ do
        -- Check Define
        it "Check unknown variable" $ do
            evalAst (beginScope []) (AST.Symbol "bar" Nothing) `shouldBe` (Left "Symbol 'bar' doesn't exist in the current or global scope", [ScopeBegin 0])
        it "Check unknown function" $ do
            evalAst (beginScope []) (AST.Symbol "foo" (Just [])) `shouldBe` (Left "Symbol 'foo' doesn't exist in the current or global scope", [ScopeBegin 0])
        it "Check variable definition" $ do
            evalAst (beginScope []) (Define "foo" (AST.Value 1)) `shouldBe` (Right Nothing, [Variable "foo" (AST.Value 1) 0, ScopeBegin 0])
        it "Check variable definition 2" $ do
            evalAst (beginScope []) (Define "bar" (Call "+" [AST.Value 1, AST.Value 5])) `shouldBe` (Right Nothing, [Variable "bar" (AST.Value 6) 0, ScopeBegin 0])
        it "Check variable usage" $ do
            evalAst [Variable "foo" (AST.Value 1) 0, ScopeBegin 0] (AST.Symbol "foo" Nothing) `shouldBe` (Right (Just (AST.Value 1)), [Variable "foo" (AST.Value 1) 0, ScopeBegin 0])
        it "Check variable usage 2" $ do
            evalAst [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]) 0, ScopeBegin 0] (AST.Symbol "bar" Nothing) `shouldBe` (Right (Just (AST.Value 6)), [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]) 0, ScopeBegin 0])
        it "Check invalid variable usage" $ do
            evalAst [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]) 0, ScopeBegin 0] (AST.Symbol "bar" (Just [Value 1])) `shouldBe` (Left "Symbol 'bar' isn't a function", [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]) 0, ScopeBegin 0])
        it "Check invalid function" $ do
            evalAst [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Boolean True]) Nothing) 0, ScopeBegin 0] (Symbol "foo" (Just [AST.Value 5])) `shouldBe` (Left "One or more parameters of binary operator '+' is invalid", [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Boolean True]) Nothing) 0, ScopeBegin 0])
        it "Check basic function definition" $ do
            evalAst (beginScope []) (Define "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing)) `shouldBe` (Right Nothing, [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0])
        it "Check basic function usage" $ do
            evalAst [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0] (Symbol "foo" (Just [AST.Value 5])) `shouldBe` (Right (Just (AST.Value 6)), [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0])
        it "Check invalid basic function eval" $ do
            evalAst [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0] (Symbol "foo" (Just [])) `shouldBe` (Right (Just (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing)), [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0])
        it "Check invalid basic function usage (too much parameters)" $ do
            evalAst [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0] (Symbol "foo" (Just [AST.Value 5, AST.Value 5, AST.Value 5])) `shouldBe` (Left "Expression takes 1 parameters, got 3", [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0])
        it "Check invalid basic function usage (define inside parameters)" $ do
            evalAst [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0] (Symbol "foo" (Just [Define "a" (AST.Value 5)])) `shouldBe` (Left "No evaluation in one or more parameters of expression", [Variable "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) Nothing) 0, ScopeBegin 0])
        it "Check multi-parameters function definition" $ do
            evalAst (beginScope []) (Define "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing], AST.Symbol "c" Nothing]) Nothing)) `shouldBe` (Right Nothing, [Variable  "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing], AST.Symbol "c" Nothing]) Nothing) 0, ScopeBegin 0])
        it "Check multi-parameters function usage" $ do
            evalAst [Variable  "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing], AST.Symbol "c" Nothing]) Nothing) 0, ScopeBegin 0] (Symbol "3+" (Just [AST.Value 5, AST.Value 6, AST.Value (-9)])) `shouldBe` (Right (Just (AST.Value 2)), [Variable  "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing], AST.Symbol "c" Nothing]) Nothing) 0, ScopeBegin 0])
    describe "Lambda evaluation tests" $ do
        -- Check Lambda usage
        it "Check +1 lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "+" [AST.Symbol "x" Nothing, AST.Value 1]) (Just [AST.Value 5])) `shouldBe` (Right (Just (AST.Value 6)), [])
        it "Check square lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "x" Nothing]) (Just [AST.Value 5])) `shouldBe` (Right (Just (AST.Value 25)), [])
        it "Check invalid lambda usage (not enough parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "x" Nothing]) (Just [])) `shouldBe` (Right (Just (FunctionValue ["x"] (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "x" Nothing]) Nothing)), [])
        it "Check invalid lambda usage (too much parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "x" Nothing]) (Just [AST.Value 5, AST.Value 5])) `shouldBe` (Left "Expression takes 1 parameters, got 2", [])
        it "Check invalid lambda usage (define inside parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "x" Nothing]) (Just [Define "a" (AST.Value 5)])) `shouldBe` (Left "No evaluation in one or more parameters of expression", [])
    describe "Cond Ast evaluation tests" $ do
        -- Check Cond
        it "Check true Cond" $ do
            evalAst [] (Cond (AST.Boolean True) (AST.Value 5) Nothing) `shouldBe` (Right (Just (AST.Value 5)), [])
        it "Check false Cond" $ do
            evalAst [] (Cond (AST.Boolean False) (AST.Value 5) Nothing) `shouldBe` (Right Nothing, [])
        it "Check true Cond else" $ do
            evalAst [] (Cond (AST.Boolean True) (AST.Value 5) (Just (AST.Value 6))) `shouldBe` (Right (Just (AST.Value 5)), [])
        it "Check false Cond else" $ do
            evalAst [] (Cond (AST.Boolean False) (AST.Value 5) (Just (AST.Value 6))) `shouldBe` (Right (Just (AST.Value 6)), [])
        it "Check invalid condition" $ do
            evalAst [] (Cond (Define "a" (AST.Value 5)) (AST.Value 6) Nothing) `shouldBe` (Left "No evaluation in condition", [])
        it "Check invalid condition 2" $ do
            evalAst [] (Cond (AST.Value 5) (AST.Value 6) Nothing) `shouldBe` (Left "Condition isn't a boolean", [])
    describe "List Ast evaluation tests" $ do
        -- Check List
        it "Check empty list" $ do
            evalAst [] (AST.List []) `shouldBe` (Right (Just (AST.List [])), [])
        it "Check non empty list" $ do
            evalAst [] (AST.List [AST.Value 5, AST.String "blegh"]) `shouldBe` (Right (Just (AST.List [AST.Value 5, AST.String "blegh"])), [])
        it "Check good index list" $ do
            evalAst [] (AST.Call "!!" [AST.List [AST.Value 5, AST.String "blegh"], AST.Value 1]) `shouldBe` (Right (Just (AST.String "blegh")), [])
        it "Check bad index list" $ do
            evalAst [] (AST.Call "!!" [AST.List [AST.Value 5, AST.String "blegh"], AST.Value 3]) `shouldBe` (Left "Index out of range", [])
        it "Check bad index list 2" $ do
            evalAst [] (AST.Call "!!" [AST.List [AST.Value 5, AST.String "blegh"], AST.Value (-1)]) `shouldBe` (Left "Index out of range", [])
        it "Check length empty list" $ do
            evalAst [] (Call "len" [AST.List []]) `shouldBe` (Right (Just (AST.Value 0)), [])
        it "Check non empty list" $ do
            evalAst [] (Call "len" [AST.List [AST.Value 5, AST.String "blegh"]]) `shouldBe` (Right (Just (AST.Value 2)), [])
        it "Check append" $ do
            evalAst [] (Call "++" [AST.List [AST.Value 5, AST.String "blegh"], AST.Value 8]) `shouldBe` (Right (Just (AST.List [AST.Value 5, AST.String "blegh", AST.Value 8])), [])
        it "Check remove occurence" $ do
            evalAst [] (Call "--" [AST.List [AST.Value 5, AST.String "blegh"], AST.Value 5]) `shouldBe` (Right (Just (AST.List [AST.String "blegh"])), [])
        it "Check remove occurence 2" $ do
            evalAst [] (Call "--" [AST.List [AST.Value 5, AST.String "blegh", AST.Value 5], AST.Value 5]) `shouldBe` (Right (Just (AST.List [AST.String "blegh"])), [])
        it "Check remove occurence 3" $ do
            evalAst [] (Call "--" [AST.List [], AST.Value 5]) `shouldBe` (Right (Just (AST.List [])), [])
        it "Check remove occurence 3" $ do
            evalAst [] (Call "--" [AST.List [AST.Value 5, AST.Value 5, AST.Value 5, AST.Value 5], AST.Value 5]) `shouldBe` (Right (Just (AST.List [])), [])
    describe "Advanced Ast evaluation tests" $ do
        -- Advanced tests
        it "Check factorial definition" $ do
            evalAst [] (Define "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x" Nothing]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x" Nothing, Symbol "fact" (Just [Call "-" [AST.Symbol "x" Nothing, AST.Value 1]])]))) Nothing)) `shouldBe` (Right Nothing, [Variable "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x" Nothing]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "fact" (Just [Call "-" [AST.Symbol "x" Nothing, AST.Value 1]])]))) Nothing) 0])
        it "Check factorial usage" $ do
            evalAst [Variable "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x" Nothing]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x" Nothing, Symbol "fact" (Just [Call "-" [AST.Symbol "x" Nothing, AST.Value 1]])]))) Nothing) 0] (Symbol "fact" (Just [AST.Value 6])) `shouldBe` (Right (Just (AST.Value 720)), [Variable "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x" Nothing]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x" Nothing, AST.Symbol "fact" (Just [Call "-" [AST.Symbol "x" Nothing, AST.Value 1]])]))) Nothing) 0])
        it "Infinite recursion" $ do
            evalAst [Variable "eh" (FunctionValue ["x"] (Symbol "eh" (Just [Symbol "x" Nothing])) Nothing) 0] (Symbol "eh" (Just [AST.Value 1])) `shouldBe` (Left "Recursion limit reached", [Variable "eh" (FunctionValue ["x"] (Symbol "eh" (Just [Symbol "x" Nothing])) Nothing) 0])
