{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- CompilerSpec
-}

module CompilerSpec where

import Test.Hspec
import Data.Maybe (Maybe(Nothing))
import qualified AST
import Compiler

spec :: Spec
spec = do
    describe "CompilerTest" $ do
        -- Push Types
        -- PushI
        it "Check astToInstructions Int not empty" $ do
            astToInstructions (AST.Value 5) `shouldNotBe` []
        it "Check astToInstructions Int success" $ do
            astToInstructions (AST.Value 5) `shouldBe` [PushI 5]
        -- PushB
        it "Check astToInstructions Boolean not empty" $ do
            astToInstructions (AST.Boolean True) `shouldNotBe` []
        it "Check astToInstructions Boolean success" $ do
            astToInstructions (AST.Boolean True)
            `shouldBe` [PushB True]
        -- PushSym
        it "Check astToInstructions Symbol not empty" $ do
            astToInstructions (AST.Symbol "foo" Nothing) `shouldNotBe` []
        it "Check astToInstructions Symbol no args" $ do
            astToInstructions (AST.Symbol "foo" Nothing)
            `shouldBe` [PushSym "foo" Nothing]
        it "Check astToInstructions Symbol with args" $ do
            astToInstructions
                (AST.Symbol "foo" (Just [AST.Value 4, AST.Value 2]))
            `shouldBe`
                [PushSym "foo"
                (Just [[PushI 4], [PushI 2]])]
        -- PushStr
        it "Check astToInstructions String not empty" $ do
            astToInstructions (AST.String "lobster") `shouldNotBe` []
        it "Check astToInstructions String success" $ do
            astToInstructions (AST.String "lobster")
            `shouldBe` [PushStr "lobster"]
        -- PushList
        it "Check astToInstructions List not empty" $ do
            astToInstructions (AST.List [AST.Value 4, AST.Value 2]) `shouldNotBe` []
        it "Check astToInstructions List populated list" $ do
            astToInstructions (AST.List [AST.Value 4, AST.Value 2])
            `shouldBe` [PushList 2 [[PushI 4], [PushI 2]]]
        it "Check astToInstructions List empty list" $ do
            astToInstructions (AST.List [])
            `shouldBe` [PushList 0 []]

        -- -- Call - User defined functions
        -- it "Check astToInstructions Call not empty" $ do
        --     astToInstructions (AST.Call "foo" [AST.Value 42]) `shouldNotBe` []
        -- it "Check astToInstructions Call user defined" $ do
        --     astToInstructions (AST.Call "foo" [AST.Value 42]) `shouldBe` [PushI 42, PushS "foo", Call]

        -- Call - Built-in functions
        -- Add
        it "Check astToInstructions Call built-in \"+\"" $ do
            astToInstructions (AST.Call "+" [AST.Value 42, AST.Value 84])
            `shouldBe` [PushI 42, PushI 84, Add]
        -- Sub
        it "Check astToInstructions Call built-in \"-\"" $ do
            astToInstructions (AST.Call "-" [AST.Value 42, AST.Value 84])
            `shouldBe` [PushI 42, PushI 84, Sub]
        -- Mul
        it "Check astToInstructions Call built-in \"*\"" $ do
            astToInstructions (AST.Call "*" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Mul]
        -- Div
        it "Check astToInstructions Call built-in \"/\"" $ do
            astToInstructions (AST.Call "/" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Div]
        -- Mod
        it "Check astToInstructions Call built-in \"%\"" $ do
            astToInstructions (AST.Call "%" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Mod]
        -- XorB
        it "Check astToInstructions Call built-in \"^^\"" $ do
            astToInstructions
                (AST.Call "^^" [AST.Boolean True, AST.Boolean False])
            `shouldBe`
                [PushB True, PushB False, XorB]
        -- Eq
        it "Check astToInstructions Call built-in \"==\"" $ do
            astToInstructions (AST.Call "==" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Eq]
        -- NotEq
        it "Check astToInstructions Call built-in \"==\"" $ do
            astToInstructions (AST.Call "!=" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, NotEq]
        -- Less
        it "Check astToInstructions Call built-in \"<\"" $ do
            astToInstructions (AST.Call "<" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Less]
        -- LessEq
        it "Check astToInstructions Call built-in \"<=\"" $ do
            astToInstructions (AST.Call "<=" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, LessEq]
        -- Great
        it "Check astToInstructions Call built-in \">\"" $ do
            astToInstructions (AST.Call ">" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, Great]
        -- GreatEq
        it "Check astToInstructions Call built-in \">=\"" $ do
            astToInstructions (AST.Call ">=" [AST.Value 42, AST.Value 2])
            `shouldBe` [PushI 42, PushI 2, GreatEq]
        -- And
        it "Check astToInstructions Call built-in \"&&\"" $ do
            astToInstructions (AST.Call "&&" [AST.Boolean True, AST.Boolean False])
            `shouldBe` [PushB True, PushB False, And]
        -- Or
        it "Check astToInstructions Call built-in \"||\"" $ do
            astToInstructions
                (AST.Call "||" [AST.Boolean True, AST.Boolean False])
            `shouldBe` [PushB True, PushB False, Or]
        -- Not
        it "Check astToInstructions Call built-in \"!\"" $ do
            astToInstructions (AST.Call "!" [AST.Boolean True])
            `shouldBe` [PushB True, Not]
        -- Then
        it "Check astToInstructions Call built-in \"$\"" $ do
            astToInstructions
                (AST.Call "$" [
                    AST.Call "+" [AST.Value 42, AST.Value 84],
                    AST.Call "-" [AST.Value 42, AST.Value 84]
                ])
            `shouldBe`
                [   PushI 42, PushI 84, Add,
                    PushI 42, PushI 84, Sub,
                    Then
                ]
        -- ToStr
        it "Check astToInstructions Call built-in \"ToStr\"" $ do
            astToInstructions (AST.Call "@" [AST.Boolean True])
            `shouldBe` [PushB True, ToStr]
        -- Apnd
        it "Check astToInstructions Call built-in \"Apnd\"" $ do
            astToInstructions (AST.Call "++" [AST.Boolean True])
            `shouldBe` [PushB True, Apnd]
        -- RemAllOcc
        it "Check astToInstructions Call built-in \"RemAllOcc\"" $ do
            astToInstructions (AST.Call "--" [AST.Boolean True])
            `shouldBe` [PushB True, RemAllOcc]
        -- Get
        it "Check astToInstructions Call built-in \"Get\"" $ do
            astToInstructions (AST.Call "!!" [AST.Boolean True])
            `shouldBe` [PushB True, Get]
        -- Len
        it "Check astToInstructions Call built-in \"Len\"" $ do
            astToInstructions (AST.Call "~" [AST.Boolean True])
            `shouldBe` [PushB True, Len]

        it "Check astToInstructions Define not empty" $ do
            astToInstructions (AST.Define "foo" (AST.Value 42))
            `shouldNotBe` []
        it "Check astToInstructions Define success" $ do
            astToInstructions (AST.Define "foo" (AST.Value 42))
            `shouldBe` [Def "foo" 1 [PushI 42]]
        it "Check astToInstructions Define with nested Define" $ do
            astToInstructions
                (AST.Define "foo" (AST.Define "bar" (AST.Value 42)))
            `shouldBe` [Def "foo" 1 [Def "bar" 1 [PushI 42]]]
        it "Check astToInstructions Define with call" $ do
            astToInstructions
                (AST.Define "foo"
                (AST.FunctionValue ["a", "b"] (AST.Call "+"
                [AST.Symbol "a" Nothing, AST.Symbol "b" Nothing]) Nothing))
            `shouldBe`
                [Def "foo" 1 [Fnv 2 ["a","b"] 4
                [PushArg 0,PushArg 1,Add,Ret] [] Nothing]]
