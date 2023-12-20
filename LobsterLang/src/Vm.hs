{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- vm
-}

module Vm (Operator(..),
           Value(..),
           Instruction(..),
           makeOperation,
           exec) where

import Stack
import Data.Ratio

data Value = IntVal Int
           | BoolVal Bool
           deriving (Show, Eq)

instance Num Value where
  (IntVal x) + (IntVal y) = IntVal (x + y)
  _ + _ = error "Unsupported operand types for +"
  (IntVal x) - (IntVal y) = IntVal (x - y)
  _ - _ = error "Unsupported operand types for -"
  (IntVal x) * (IntVal y) = IntVal (x * y)
  _ * _ = error "Unsupported operand types for *"
  abs (IntVal x) = IntVal (abs x)
  abs _ = error "Unsupported operand type for abs"
  signum (IntVal x) = IntVal (signum x)
  signum _ = error "Unsupported operand type for signum"
  fromInteger x = IntVal (fromInteger x)

instance Fractional Value where
  (IntVal x) / (IntVal y) = IntVal (x `div` y)
  _ / _ = error "Unsupported operand types for /"
  fromRational x = IntVal (fromInteger (numerator x) `div` fromInteger (denominator x))


data Operator = Add
              | Subtract
              | Multiply
              | Divide

data Instruction = Push Value
                | Call Operator
                | Ret

type Stack = [Value]
type Inst = [Instruction]

makeOperation :: Operator -> Stack -> Either String Stack
makeOperation Add stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x + y))
        (Nothing, _) -> Left "Error : Add need two arguments"
    (Nothing, _) -> Left "Error : Add need two arguments"
makeOperation Subtract stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x - y))
        (Nothing, _) -> Left "Error : Subtract need two arguments"
    (Nothing, _) -> Left "Error : Subtract need two arguments"
makeOperation Multiply stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x * y))
        (Nothing, _) -> Left "Error : Multiply need two arguments"
    (Nothing, _) -> Left "Error : Multiply need two arguments"
makeOperation Divide stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | y /= 0 -> Right (Stack.push stack2 (x / y))
            | otherwise -> Left "Error: division by zero"
        (Nothing, _) -> Left "Error : Divide need two arguments"
    (Nothing, _) -> Left "Error : Divide need two arguments"



exec :: Inst -> Stack -> Either String Value
exec (Call _:_) [] = Left "Error: stack is empty"
exec (Call op :xs) stack = case makeOperation op stack of
    Left err -> Left err
    Right newstack -> exec xs newstack
exec (Push val : xs) stack = exec xs (Stack.push stack val)
exec (Ret : _) stack = case Stack.top stack of
    Just x -> Right x
    Nothing -> Left "Error: stack is empty"
exec [] _ = Left "list no instruction found"
