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
    -- it "Check parseTuple Int Success" $ do
    --     parseTuple parseInt "(-123,456)Hello" `shouldBe` Just ((-123,456), "Hello")
    -- it "Check parseTuple Int first Failure" $ do
    --     parseTuple parseInt "(oui,42)Hello" `shouldBe` Nothing
    -- it "Check parseTuple Int second Failure" $ do
    --     parseTuple parseInt "(-42,oui)Hello" `shouldBe` Nothing
    -- it "Check parseTuple UInt Success" $ do
    --     parseTuple parseUInt "(123,456)Hello" `shouldBe` Just ((123,456), "Hello")
    -- it "Check parseTuple UInt first Failure" $ do
    --     parseTuple parseUInt "(non,42)Hello" `shouldBe` Nothing
    -- it "Check parseTuple UInt second Failure" $ do
    --     parseTuple parseUInt "(42,non)Hello" `shouldBe` Nothing
    -- it "Check parseTuple AnyChar first Success" $ do
    --     parseTuple (parseSome (parseAnyChar ['a'..'z'])) "(bon,jour)Hello" `shouldBe` Just (("bon","jour"), "Hello")
    -- it "Check parseTuple AnyChar second Success" $ do
    --     parseTuple (parseSome (parseAnyChar (['A'..'Z'] ++ ['a'..'z'] ++ "' "))) "(C'est cool Epitech,J'rigole)Hello" `shouldBe` Just (("C'est cool Epitech", "J'rigole"), "Hello")
    -- it "Check parseTuple AnyChar Failure" $ do
    --     parseTuple (parseAnyChar ['a'..'z']) "(42,42)Hello" `shouldBe` Nothing
    -- it "Check parseTuple missing '()' Failure" $ do
    --     parseTuple (parseAnyChar ['a'..'z']) "ouiHello" `shouldBe` Nothing
    -- it "Check parseTuple missing '(' Failure" $ do
    --     parseTuple (parseAnyChar ['a'..'z']) "oui,non)Hello" `shouldBe` Nothing
    -- it "Check parseTuple missing ')' Failure" $ do
    --     parseTuple (parseAnyChar ['a'..'z']) "(oui,nonHello" `shouldBe` Nothing
    
    
    -- "(define vie 42)"
    -- "(define (fact x))"
    -- "(+ (* 2 3) (div 10 2))"
    -- it "returns the first element of a list" $ do
    --   head [23 ..] `shouldBe` (23 :: Int)

    -- it "returns the first element of an *arbitrary* list" $
    --   property $ \x xs -> head (x:xs) == (x :: Int)

    -- it "throws an exception if used with an empty list" $ do
    --   evaluate (head []) `shouldThrow` anyException
