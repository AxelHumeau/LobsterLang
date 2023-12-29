{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- ParseSpec
-}

module ParseSpec where

import Test.Hspec
import Parse
import qualified SExpr
import qualified AST
import Data.Bool (Bool(True, False))

spec :: Spec
spec = do
    describe "ParseTest" $ do
        it "Check parseChar Success" $ do
            runParser (parseChar ' ') " Hello" `shouldBe` Just (' ', "Hello")
        it "Check parseChar Failure" $ do
            runParser (parseChar ' ') "Hello" `shouldBe` Nothing
        it "Check parseOr Success first arg" $ do
            runParser (parseOr (parseChar 'a') (parseChar ' ')) "aHello" `shouldBe` Just ('a', "Hello")
        it "Check parseOr Success second arg" $ do
            runParser (parseOr (parseChar 'a') (parseChar ' ')) " Hello" `shouldBe` Just (' ', "Hello")
        it "Check parseOr Failure" $ do
            runParser (parseOr (parseChar 'f') (parseChar 'O')) " Oui" `shouldBe` Nothing
        it "Check parseAnd Success" $ do
            runParser (parseAnd (parseChar 'a') (parseChar 'p')) "apHello" `shouldBe` Just (('a', 'p'), "Hello")
        it "Check parseAnd Failure" $ do
            runParser (parseAnd (parseChar 'e') (parseChar 'p')) "apHello" `shouldBe` Nothing
        it "Check parseAndWith Number Success" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['0'..'9']) (parseAnyChar ['0'..'9'])) "42Hello" `shouldBe` Just ("42", "Hello")
        it "Check parseAndWith Character Success" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['a'..'z']) (parseAnyChar ['a'..'z'])) "ohHello" `shouldBe` Just ("oh", "Hello")
        it "Check parseAndWith Failure" $ do
            runParser (parseAndWith (\x y -> [x, y]) (parseAnyChar ['a'..'z']) (parseAnyChar ['0'..'9'])) "42Hello" `shouldBe` Nothing
        it "Check parseMany Character Success" $ do
            runParser (parseMany (parseAnyChar ['a'..'z'])) "bonjournoHello" `shouldBe` Just ("bonjourno", "Hello")
        it "Check parseMany Number Success" $ do
            runParser (parseMany (parseAnyChar ['0'..'9'])) "424554Hello" `shouldBe` Just ("424554", "Hello")
        it "Check parseMany Failure" $ do
            runParser (parseMany (parseAnyChar ['0'..'9'])) "Hello" `shouldBe` Just ("", "Hello")
        it "Check parseSome Number Success" $ do
            runParser (parseSome (parseAnyChar ['0'..'9'])) "042Hello" `shouldBe` Just ("042", "Hello")
        it "Check parseSome Character Success" $ do
            runParser (parseSome (parseAnyChar ['a'..'z'])) "buenos42Hello" `shouldBe` Just ("buenos", "42Hello")
        it "Check parseSome Failure" $ do
            runParser (parseSome (parseAnyChar ['0'..'9'])) "HelloWorld" `shouldBe` Nothing
        it "Check parseUInt Success" $ do
            runParser parseUInt "5463Hello" `shouldBe` Just (5463, "Hello")
        it "Check parseUInt Failure" $ do
            runParser parseUInt "Hola" `shouldBe` Nothing
        it "Check parseUInt Empty" $ do
            runParser parseUInt "" `shouldBe` Nothing
        it "Check parseUInt Negative value Failure" $ do
            runParser parseUInt "-42Hello" `shouldBe` Nothing
        it "Check parseInt Success" $ do
            runParser parseInt "4234Hello" `shouldBe` Just (4234, "Hello")
        it "Check parseInt Negative value Success" $ do
            runParser parseInt "-42Hello" `shouldBe` Just (-42, "Hello")
        it "Check parseInt Failure" $ do
            runParser parseInt "Hello" `shouldBe` Nothing
        it "Check parsesign '-' Success" $ do
            runParser parseSign "-llg" `shouldBe` Just ('-', "llg")
        it "Check parsesign '+' Success" $ do
            runParser parseSign "+llg" `shouldBe` Just ('+', "llg")
        it "Check parsesign Failure" $ do
            runParser parseSign "lg" `shouldBe` Nothing
        it "Check parseString Success n°1" $ do
            runParser parseString "bonjourno " `shouldBe` Just ("bonjourno", "")
        it "Check parseString Success n°2" $ do
            runParser parseString "bon12*/p journo " `shouldBe` Just ("bon", "12*/p journo ")
        it "Check parseString Failure" $ do
            runParser parseString "^bon12*/p journo " `shouldBe` Nothing
        it "Check parseElem with parseInt Success" $ do
            runParser (parseElem parseInt) "12 " `shouldBe` Just (12, "")
        it "Check parseElem with parseString Success" $ do
            runParser (parseElem parseString) "hello la " `shouldBe` Just ("hello", "la ")
        it "Check parseElem with parseSymbol Success" $ do
            runParser (parseElem parseSymbol) "hello   la " `shouldBe` Just (SExpr.Symbol "hello", "la ")
        it "Check parseValue Success" $ do
            runParser parseValue "432           la " `shouldBe` Just (SExpr.Value 432, "la ")
        it "Check parseSymbol Success" $ do
            runParser parseSymbol "symbol           la " `shouldBe` Just (SExpr.Symbol "symbol", "la ")
        it "Check parseList with parseInt Success" $ do
            runParser (parseList parseInt) "(1 2 3   4 5) " `shouldBe` Just ([1, 2 ,3 , 4, 5], "")
        it "Check parseList with parseInt Failure (without a number inside)" $ do
            runParser (parseList parseInt) "(1 2 3  d 4 5) " `shouldBe` Nothing
        it "Check parseList with parseInt Failure (without a ending ')')" $ do
            runParser (parseList parseInt) "(1 2 3  4 5 " `shouldBe` Nothing
        it "Check parseList with parseInt Failure (without a starting '(')" $ do
            runParser (parseList parseInt) "1 2 3  4 5)" `shouldBe` Nothing
        it "Check parseList with parseString Success" $ do
            runParser (parseList parseString) "(buenos owow k ye    )1 2 3  4 5)" `shouldBe` Just (["buenos", "owow", "k", "ye"], "1 2 3  4 5)")
        it "Check parseList with parseString Failure" $ do
            runParser (parseList parseString) "(buenos 3 owow k ye    )1 2 3  4 5)" `shouldBe` Nothing
        it "Check parseBool true Success" $ do
            runParser parseBool "#t lp" `shouldBe` Just (True, "lp")
        it "Check parseBool false Success" $ do
            runParser parseBool "#f lp" `shouldBe` Just (False, "lp")
        it "Check parseBool Failure" $ do
            runParser parseBool "#tlp" `shouldBe` Nothing
        it "Check parseSExpr Success n°1" $ do
            runParser parseSExpr "(define foo (* 3 3))" `shouldBe` Just (SExpr.List [SExpr.Symbol "define",SExpr.Symbol "foo",SExpr.List [SExpr.Symbol "*",SExpr.Value 3,SExpr.Value 3]], "")
        it "Check parseSExpr Success n°2" $ do
            runParser parseSExpr "(   define   foo    3     )" `shouldBe` Just (SExpr.List [SExpr.Symbol "define",SExpr.Symbol "foo",SExpr.Value 3], "")
        it "Check ParseLisp Success n°1" $ do
            parseLisp "(* 3 (+ 2 2))" `shouldBe` (Just (AST.Value 12), [])
        it "Check ParseLisp Success n°2" $ do
            parseLisp "(* 3 (+ 2 (/ 12 6)))" `shouldBe` (Just (AST.Value 12), [])
        it "Check ParseLisp Success n°2" $ do
            parseLisp "(* 3 (+ 2 (/ 12 6))" `shouldBe` (Nothing, [])
