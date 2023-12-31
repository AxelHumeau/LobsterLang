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
            evalAst [] (AST.Value 5) `shouldBe` (Right (Just (AST.Value 5)), [])
        it "Check Boolean True" $ do
            evalAst [] (AST.Boolean True) `shouldBe` (Right (Just (AST.Boolean True)), [])
        it "Check Boolean False" $ do
            evalAst [] (AST.Boolean False) `shouldBe` (Right (Just (AST.Boolean False)), [])
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
            evalBiValOp (+) [] (Call "+" [AST.Value 8]) `shouldBe` (Left "Not enough parameter for binary operator '+'", [])
        it "Check invalid value binary operation (too much ast parameters)" $ do
            evalBiValOp (+) [] (Call "+" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameter for binary operator '+'", [])
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
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8]) `shouldBe` (Left "Not enough parameter for binary operator '=='", [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiCompValOp (==) [] (Call "==" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameter for binary operator '=='", [])
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
        it "Check invalid operation ! (not)" $ do
            evalAst [] (Call "!" [AST.Boolean True, AST.Boolean False]) `shouldBe` (Left "Invalid number of parameter for unary operator '!'", [])
        it "Check invalid operation ! (not) 2" $ do
            evalAst [] (Call "!" [AST.Value 5]) `shouldBe` (Left "Parameter of unary operator '!' isn't a boolean", [])
        it "Check invalid value comparison binary operation (wrong type)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Boolean True, AST.Value 8]) `shouldBe` (Left "One or more parameters of binary operator '&&' is invalid", [])
        it "Check invalid value comparison binary operation (wrong type 2)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Boolean False]) `shouldBe` (Left "One or more parameters of binary operator '&&' is invalid", [])
        it "Check invalid value comparison binary operation (not enough ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8]) `shouldBe` (Left "Not enough parameter for binary operator '&&'", [])
        it "Check invalid value comparison binary operation (too much ast parameters)" $ do
            evalBiBoolOp (&&) [] (Call "&&" [AST.Value 8, AST.Value 9, AST.Value 3]) `shouldBe` (Left "Too much parameter for binary operator '&&'", [])
        -- Check Define
        it "Check unknown variable" $ do
            evalAst (beginScope []) (AST.Symbol "bar") `shouldBe` (Left "Variable 'bar' doesn't exist", [ScopeBegin])
        it "Check unknown function" $ do
            evalAst (beginScope []) (AST.Call "foo" []) `shouldBe` (Left "Function 'foo' not found", [ScopeBegin])
        it "Check variable definition" $ do
            evalAst (beginScope []) (Define "foo" (AST.Value 1)) `shouldBe` (Right Nothing, [Variable "foo" (AST.Value 1), ScopeBegin])
        it "Check variable definition 2" $ do
            evalAst (beginScope []) (Define "bar" (Call "+" [AST.Value 1, AST.Value 5])) `shouldBe` (Right Nothing, [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin])
        it "Check variable usage" $ do
            evalAst [Variable "foo" (AST.Value 1), ScopeBegin] (AST.Symbol "foo") `shouldBe` (Right (Just (AST.Value 1)), [Variable "foo" (AST.Value 1), ScopeBegin])
        it "Check variable usage 2" $ do
            evalAst [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin] (AST.Symbol "bar") `shouldBe` (Right (Just (AST.Value 6)), [Variable "bar" (Call "+" [AST.Value 1, AST.Value 5]), ScopeBegin])
        it "Check invalid function" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Boolean True]), ScopeBegin] (Call "foo" [AST.Value 5]) `shouldBe` (Left "One or more parameters of binary operator '+' is invalid", [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Boolean True]), ScopeBegin])
        it "Check basic function definition" $ do
            evalAst (beginScope []) (Define "foo" (FunctionValue ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]) Nothing)) `shouldBe` (Right Nothing, [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check basic function usage" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin] (Call "foo" [AST.Value 5]) `shouldBe` (Right (Just (AST.Value 6)), [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check invalid basic function usage (not enough parameters)" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin] (Call "foo" []) `shouldBe` (Left "Function 'foo' takes 1 parameters, got 0", [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check invalid basic function usage (too much parameters)" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin] (Call "foo" [AST.Value 5, AST.Value 5, AST.Value 5]) `shouldBe` (Left "Function 'foo' takes 1 parameters, got 3", [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check invalid basic function usage (define inside parameters)" $ do
            evalAst [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin] (Call "foo" [Define "a" (AST.Value 5)]) `shouldBe` (Left "No evaluation in one or more parameters of 'foo'", [Function "foo" ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]), ScopeBegin])
        it "Check multi-parameters function definition" $ do
            evalAst (beginScope []) (Define "3+" (FunctionValue ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]) Nothing)) `shouldBe` (Right Nothing, [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin])
        it "Check multi-parameters function usage" $ do
            evalAst [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin] (Call "3+" [AST.Value 5, AST.Value 6, AST.Value (-9)]) `shouldBe` (Right (Just (AST.Value 2)), [Function "3+" ["a", "b", "c"] (Call "+" [AST.Call "+" [AST.Symbol "a", AST.Symbol "b"], AST.Symbol "c"]), ScopeBegin])
        -- Check Lambda usage
        it "Check +1 lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "+" [AST.Symbol "x", AST.Value 1]) (Just [AST.Value 5])) `shouldBe` (Right (Just (AST.Value 6)), [])
        it "Check square lambda" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) (Just [AST.Value 5])) `shouldBe` (Right (Just (AST.Value 25)), [])
        it "Check invalid lambda usage (not enough parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) (Just [])) `shouldBe` (Left "Lambda takes 1 parameters, got 0", [])
        it "Check invalid lambda usage (too much parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) (Just [AST.Value 5, AST.Value 5])) `shouldBe` (Left "Lambda takes 1 parameters, got 2", [])
        it "Check invalid lambda usage (define inside parameters)" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) (Just [Define "a" (AST.Value 5)])) `shouldBe` (Left "No evaluation in one or more parameters of lambda", [])
        it "Check lambda not used" $ do
            evalAst [] (FunctionValue ["x"] (Call "*" [AST.Symbol "x", AST.Symbol "x"]) Nothing) `shouldBe` (Right Nothing, [])
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
        -- Advanced tests
        it "Check factorial definition" $ do
            evalAst [] (Define "fact" (FunctionValue ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]]))) Nothing)) `shouldBe` (Right Nothing, [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))])
        it "Check factorial usage" $ do
            evalAst [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))] (Call "fact" [AST.Value 6]) `shouldBe` (Right (Just (AST.Value 720)), [Function "fact" ["x"] (Cond (Call "==" [AST.Value 0, AST.Symbol "x"]) (AST.Value 1) (Just (Call "*" [AST.Symbol "x", Call "fact" [Call "-" [AST.Symbol "x", AST.Value 1]]])))])
