{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- ParseSpec
-}

module ParseSpec where

import Test.Hspec
import Parse

spec :: Spec
spec = do
    describe "Parser Tests" $ do
        it "Check parseList String Success" $ do
            runParser (parseList parseString) (0,0) "(| a,  b ,c, d |)" `shouldBe` Right(["a", "b", "c", "d"], "", (0,17))
        it "Check parseList Error missing comma" $ do
            runParser (parseList parseString) (0,0) "(| a b |)" `shouldBe` Left "Error on parsing on '0' '5'"
        it "Check parseList Error end with comma" $ do
            runParser (parseList parseString) (0,0) "(|a, |)" `shouldBe` Left "Error on parsing on '0' '5'"
        it "Check parseList Error starting with comma" $ do
            runParser (parseList parseString) (0,0) "(|,a|)" `shouldBe` Left "Error on parsing on '0' '2'"
        it "Check parseList Error missing starting bracket" $ do
            runParser (parseList parseString) (0,0) "a, b|)" `shouldBe` Left "Error on parsing on '0' '0'"
        it "Check parseList Error missing ending bracket" $ do
            runParser (parseList parseString) (0,0) "(|a, b" `shouldBe` Left "Error on parsing on '0' '6'"
