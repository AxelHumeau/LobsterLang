{-
-- EPITECH PROJECT, 2023
-- Spec.hs
-- File description:
-- Spec
-}

import Test.Hspec
import Parse
import Data.Maybe()

main :: IO ()
main = hspec $ do
  describe "ParseTest" $ do
    it "Check parseChar Success" $ do
        parseChar ' ' " Hello" `shouldBe` Just (' ', "Hello")
    it "Check parseChar Failure" $ do
        parseChar ' ' "Hello" `shouldBe` Nothing
    it "Check parseOr Success first arg" $ do
        parseOr (initParseChar 'a') (initParseChar ' ') "aHello" `shouldBe` Just ('a', "Hello")
    it "Check parseOr Success second arg" $ do
        parseOr (initParseChar 'a') (initParseChar ' ') " Hello" `shouldBe` Just (' ', "Hello")
    it "Check parseOr Failure" $ do
        parseOr (initParseChar 'f') (initParseChar 'O') " Oui" `shouldBe` Nothing
    it "Check parseAnd Success" $ do
        parseAnd (initParseChar 'a') (initParseChar 'p') "apHello" `shouldBe` Just (('a', 'p'), "Hello")
    it "Check parseAnd Failure" $ do
        parseAnd (initParseChar 'e') (initParseChar 'p') "apHello" `shouldBe` Nothing
    it "Check parseAndWith Number Success" $ do
        parseAndWith (\x y -> [x, y]) (initParseAnyChar ['0'..'9']) (initParseAnyChar ['0'..'9']) "42Hello" `shouldBe` Just ("42", "Hello")
    it "Check parseAndWith Character Success" $ do
        parseAndWith (\x y -> [x, y]) (initParseAnyChar ['a'..'z']) (initParseAnyChar ['a'..'z']) "ohHello" `shouldBe` Just ("oh", "Hello")
    it "Check parseAndWith Failure" $ do
        parseAndWith (\x y -> [x, y]) (initParseAnyChar ['a'..'z']) (initParseAnyChar ['0'..'9']) "42Hello" `shouldBe` Nothing
    it "Check parseMany Character Success" $ do
        parseMany (initParseAnyChar ['a'..'z']) "bonjournoHello" `shouldBe` Just ("bonjourno", "Hello")
    it "Check parseMany Number Success" $ do
        parseMany (initParseAnyChar ['0'..'9']) "424554Hello" `shouldBe` Just ("424554", "Hello")
    it "Check parseMany Failure" $ do
        parseMany (initParseAnyChar ['0'..'9']) "Hello" `shouldBe` Just ("", "Hello")
    it "Check parseSome Number Success" $ do
        parseSome (initParseAnyChar ['0'..'9']) "042Hello" `shouldBe` Just ("042", "Hello")
    it "Check parseSome Character Success" $ do
        parseSome (initParseAnyChar ['a'..'z']) "buenos42Hello" `shouldBe` Just ("buenos", "42Hello")
    it "Check parseSome Failure" $ do
        parseSome (initParseAnyChar ['0'..'9']) "HelloWorld" `shouldBe` Nothing
    it "Check parseUInt Success" $ do
        parseUInt "5463Hello" `shouldBe` Just (5463, "Hello")
    it "Check parseUInt Failure" $ do
        parseUInt "Hola" `shouldBe` Nothing
    it "Check parseUInt Empty" $ do
        parseUInt "" `shouldBe` Nothing
    it "Check parseUInt Negative value Failure" $ do
        parseUInt "-42Hello" `shouldBe` Nothing
    it "Check parseInt Success" $ do
        parseInt "4234Hello" `shouldBe` Just (4234, "Hello")
    it "Check parseInt Negative value Success" $ do
        parseInt "-42Hello" `shouldBe` Just (-42, "Hello")
    it "Check parseInt Failure" $ do
        parseInt "Hello" `shouldBe` Nothing
    it "Check parseTuple Int Success" $ do
        parseTuple initParseInt "(-123,456)Hello" `shouldBe` Just ((-123,456), "Hello")
    it "Check parseTuple Int first Failure" $ do
        parseTuple initParseInt "(oui,42)Hello" `shouldBe` Nothing
    it "Check parseTuple Int second Failure" $ do
        parseTuple initParseInt "(-42,oui)Hello" `shouldBe` Nothing
    it "Check parseTuple UInt Success" $ do
        parseTuple initParseUInt "(123,456)Hello" `shouldBe` Just ((123,456), "Hello")
    it "Check parseTuple UInt first Failure" $ do
        parseTuple initParseUInt "(non,42)Hello" `shouldBe` Nothing
    it "Check parseTuple UInt second Failure" $ do
        parseTuple initParseUInt "(42,non)Hello" `shouldBe` Nothing
    it "Check parseTuple AnyChar first Success" $ do
        parseTuple (initParseSome (initParseAnyChar ['a'..'z'])) "(bon,jour)Hello" `shouldBe` Just (("bon","jour"), "Hello")
    it "Check parseTuple AnyChar second Success" $ do
        parseTuple (initParseSome (initParseAnyChar (['A'..'Z'] ++ ['a'..'z'] ++ "' "))) "(C'est cool Epitech,J'rigole)Hello" `shouldBe` Just (("C'est cool Epitech", "J'rigole"), "Hello")
    it "Check parseTuple AnyChar Failure" $ do
        parseTuple (initParseAnyChar ['a'..'z']) "(42,42)Hello" `shouldBe` Nothing
    it "Check parseTuple missing '()' Failure" $ do
        parseTuple (initParseAnyChar ['a'..'z']) "ouiHello" `shouldBe` Nothing
    it "Check parseTuple missing '(' Failure" $ do
        parseTuple (initParseAnyChar ['a'..'z']) "oui,non)Hello" `shouldBe` Nothing
    it "Check parseTuple missing ')' Failure" $ do
        parseTuple (initParseAnyChar ['a'..'z']) "(oui,nonHello" `shouldBe` Nothing
    
    
    -- "(define vie 42)"
    -- "(define (fact x))"
    -- "(+ (* 2 3) (div 10 2))"
    -- it "returns the first element of a list" $ do
    --   head [23 ..] `shouldBe` (23 :: Int)

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
