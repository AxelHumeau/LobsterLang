{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- CompilerSpec
-}

module CompilerSpec where

import Test.Hspec
import Compiler
import qualified AST

spec :: Spec
spec = do
    describe "CompilerTest" $ do
        it "Check astToInstructions Value not empty" $ do
            astToInstructions (AST.Value 5) `shouldNotBe` []
        it "Check astToInstructions Value success" $ do
            astToInstructions (AST.Value 5) `shouldBe` [PushI 5]
        it "Check astToInstructions Boolean not empty" $ do
            astToInstructions (AST.Boolean True) `shouldNotBe` []
        it "Check astToInstructions Boolean success" $ do
            astToInstructions (AST.Boolean True) `shouldBe` [PushB True]
        it "Check astToInstructions Symbol not empty" $ do
            astToInstructions (AST.Symbol "foo") `shouldNotBe` []
        it "Check astToInstructions Symbol success" $ do
            astToInstructions (AST.Symbol "foo") `shouldBe` [PushS "foo"]

        -- Call - User defined functions
        it "Check astToInstructions Call not empty" $ do
            astToInstructions (AST.Call "foo" [(AST.Value 42)]) `shouldNotBe` []
        it "Check astToInstructions Call user defined" $ do
            astToInstructions (AST.Call "foo" [(AST.Value 42)]) `shouldBe` [PushI 42, PushS "foo", Call]

        -- Call - Built-in functions
        it "Check astToInstructions Call built-in \"+\"" $ do
            astToInstructions (AST.Call "+" [(AST.Value 42), (AST.Value 84)]) `shouldBe` [PushI 42, PushI 84, Add]
        it "Check astToInstructions Call built-in \"-\"" $ do
            astToInstructions (AST.Call "-" [(AST.Value 42), (AST.Value 84)]) `shouldBe` [PushI 42, PushI 84, Sub]
        it "Check astToInstructions Call built-in \"-\" (Symbol Negation)" $ do
            astToInstructions (AST.Call "-" [(AST.Symbol "foo")]) `shouldBe` [PushS "foo", Neg]
        it "Check astToInstructions Call built-in \"!\" (Bool Invertion)" $ do
            astToInstructions (AST.Call "!" [(AST.Boolean True)]) `shouldBe` [PushB False]
        it "Check astToInstructions Call built-in \"*\"" $ do
            astToInstructions (AST.Call "*" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Mul]
        it "Check astToInstructions Call built-in \"/\"" $ do
            astToInstructions (AST.Call "/" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Div]
        it "Check astToInstructions Call built-in \"%\"" $ do
            astToInstructions (AST.Call "%" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Mod]

        it "Check astToInstructions Call built-in \"==\"" $ do
            astToInstructions (AST.Call "==" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Eq]
        it "Check astToInstructions Call built-in \"<\"" $ do
            astToInstructions (AST.Call "<" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Less]
        it "Check astToInstructions Call built-in \"<=\"" $ do
            astToInstructions (AST.Call "<=" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, LessEq]
        it "Check astToInstructions Call built-in \">\"" $ do
            astToInstructions (AST.Call ">" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, Great]
        it "Check astToInstructions Call built-in \">=\"" $ do
            astToInstructions (AST.Call ">=" [(AST.Value 42), (AST.Value 2)]) `shouldBe` [PushI 42, PushI 2, GreatEq]

        it "Check astToInstructions Call built-in \"&&\"" $ do
            astToInstructions (AST.Call "&&" [(AST.Boolean True), (AST.Boolean False)]) `shouldBe` [PushB True, PushB False, And]
        it "Check astToInstructions Call built-in \"||\"" $ do
            astToInstructions (AST.Call "||" [(AST.Boolean True), (AST.Boolean False)]) `shouldBe` [PushB True, PushB False, Or]

        it "Check astToInstructions Define not empty" $ do
            astToInstructions (AST.Define "foo" (AST.Value 42)) `shouldNotBe` []
        it "Check astToInstructions Define success" $ do
            astToInstructions (AST.Define "foo" (AST.Value 42)) `shouldBe` [Def "foo" 1 [PushI 42]]
        it "Check astToInstructions Define with nested Define" $ do
            astToInstructions (AST.Define "foo" (AST.Define "bar" (AST.Value 42))) `shouldBe` [Def "foo" 1 [Def "bar" 1 [PushI 42]]]
        it "Check astToInstructions Define with call" $ do
            astToInstructions (AST.Define "foo" (AST.Call "func" [AST.Value 42])) `shouldBe` [Def "foo" 3 [PushI 42, PushS "func", Call]]
