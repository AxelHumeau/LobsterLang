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

makeOperation :: Operator -> Stack -> Stack
makeOperation Add stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Stack.push stack2 (x + y)
        (Nothing, _) -> stack
    (Nothing, _) -> stack
makeOperation Subtract stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Stack.push stack2 (x - y)
        (Nothing, _) -> stack
    (Nothing, _) -> stack
makeOperation Multiply stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Stack.push stack2 (x * y)
        (Nothing, _) -> stack
    (Nothing, _) -> stack
makeOperation Divide stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Stack.push stack2 (x / y)
        (Nothing, _) -> stack
    (Nothing, _) -> stack



exec :: Inst -> Stack -> Either String Value
exec (Call op :xs) stack = exec xs (makeOperation op stack)
exec (Push val : xs) stack = exec xs (Stack.push stack val)
exec (Ret : _) stack = case Stack.top stack of
    Just x -> Right x
    Nothing -> error "error: stack is empty"
exec [] _ = error "list no instruction found"
