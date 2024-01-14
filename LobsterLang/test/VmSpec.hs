{-
-- EPITECH PROJECT, 2024
-- LobsterLang
-- File description:
-- VmSpec
-}

module VmSpec where

import Data.Bool()
import Test.Hspec
import Vm

spec :: Spec
spec = do
    describe "Vmtest" $ do
        it "Check exec Call Add" $ do
            exec [] [] [Push (IntVal 3), Push (IntVal 4), Push (Op Add), Call, Ret] [] `shouldBe` Right (IntVal 7)
        it "Check exec Call Add not enougth Arg" $ do
            exec [] [] [Push (IntVal 4), Push (Op Add), Call, Ret] [] `shouldBe` Left "Error : Add need two arguments"
        it "Check exec Call Subtract" $ do
            exec [] [] [Push (IntVal 10), Push (IntVal 5), Push (Op Vm.Sub), Call, Ret] [] `shouldBe` Right (IntVal (-5))
        it "Check exec Call Multiply" $ do
            exec [] [] [Push (IntVal 3), Push (IntVal 7), Push (Op Vm.Mul), Call, Ret] [] `shouldBe` Right (IntVal 21)
        it "Check exec Call Divide" $ do
            exec [] [] [Push (IntVal 4), Push (IntVal 20), Push (Op Vm.Div), Call, Ret] [] `shouldBe` Right (IntVal 5)
        it "Check exec Call Divide by Zero" $ do
            exec [] [] [Push (IntVal 0), Push (IntVal 10), Push (Op Vm.Div), Call, Ret] [] `shouldBe` Left "Error: division by zero"
        it "Check exec Call Equality True" $ do
            exec [] [] [Push (IntVal 5), Push (IntVal 5), Push (Op Eq), Call, Ret] [] `shouldBe` Right (BoolVal True)
        it "Check exec Call Equality False" $ do
            exec [] [] [Push (IntVal 3), Push (IntVal 7), Push (Op Eq), Call, Ret] [] `shouldBe` Right (BoolVal False)
        it "Check exec JumpIfFalse True" $ do
            exec [] [] [Push (BoolVal True), JumpIfFalse 2, Push (IntVal 10), Ret] [] `shouldBe` Right (IntVal 10)
        it "Check exec JumpIfFalse False" $ do
            exec [] [] [Push (IntVal 10), Push (IntVal 9), Push (Op Eq), Call, JumpIfFalse 2, Push (IntVal 20), Ret, Push (IntVal 69), Ret] [] `shouldBe` Right (IntVal 69)
        it "Check exec JumpIfFalse Invalid Jump" $ do
            exec [] [] [Push (BoolVal True), JumpIfFalse 5, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: invalid jump value"
        it "Check exec JumpIfFalse Stack Empty" $ do
            exec [] [] [JumpIfFalse 2, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: stack is empty"
        it "Check exec JumpIfFalse Stack Not Bool" $ do
            exec [] [] [Push (IntVal 5), JumpIfFalse 2, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: not bool"
        it "Check exec JumpIfTrue True" $ do
            exec [] [] [Push (BoolVal True), JumpIfTrue 2, Push (IntVal 10), Ret] [] `shouldBe` Left "list no instruction found"
        it "Check exec JumpIfTrue False" $ do
            exec [] [] [Push (BoolVal False), JumpIfTrue 2, Push (IntVal 20), Ret] [] `shouldBe` Right (IntVal 20)
        it "Check exec JumpIfTrue Invalid Jump" $ do
            exec [] [] [Push (BoolVal True), JumpIfTrue 5, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: invalid jump value"
        it "Check exec JumpIfTrue Stack Empty" $ do
            exec [] [] [JumpIfTrue 2, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: stack is empty"
        it "Check exec JumpIfTrue Stack Not Bool" $ do
            exec [] [] [Push (IntVal 5), JumpIfTrue 2, Push (IntVal 10), Ret] [] `shouldBe` Left "Error: not bool"
