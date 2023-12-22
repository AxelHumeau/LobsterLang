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
           deriving (Show, Eq, Ord)

instance Num Value where
  (BoolVal x) + (IntVal y) = IntVal (fromEnum x + y)
  (IntVal x) + (BoolVal y) = IntVal (x + fromEnum y)
  (IntVal x) + (IntVal y) = IntVal (x + y)
  (BoolVal x) + (BoolVal y) = IntVal (fromEnum x + fromEnum y)
  (IntVal x) - (IntVal y) = IntVal (x - y)
  (BoolVal x) - (IntVal y) = IntVal (fromEnum x - y)
  (IntVal x) - (BoolVal y) = IntVal (x - fromEnum y)
  (BoolVal x) - (BoolVal y) = IntVal (fromEnum x - fromEnum y)
  (IntVal x) * (IntVal y) = IntVal (x * y)
  (BoolVal x) * (IntVal y) = IntVal (fromEnum x * y)
  (IntVal x) * (BoolVal y) = IntVal (x * fromEnum y)
  (BoolVal x) * (BoolVal y) = IntVal (fromEnum x * fromEnum y)
  abs (IntVal x) = IntVal (abs x)
  abs (BoolVal x) = IntVal (abs (fromEnum x))
  signum (IntVal x) = IntVal (signum x)
  signum (BoolVal x) = IntVal (signum (fromEnum x))
  fromInteger x = IntVal (fromInteger x)

instance Fractional Value where
  (IntVal x) / (IntVal y) = IntVal (x `div` y)
  (BoolVal x) / (IntVal y) = IntVal (fromEnum x `div` y)
  (IntVal x) / (BoolVal y) = IntVal (x `div` fromEnum y)
  (BoolVal x) / (BoolVal y) = IntVal (fromEnum x `div` fromEnum y)
  fromRational x = IntVal (fromInteger (numerator x) `div` fromInteger (denominator x))


data Operator = Add
              | Subtract
              | Multiply
              | Divide
              | Eq
              | Less

data Instruction = Push Value
                | Call Operator
                | JumpIfFalse Int
                | JumpIfTrue Int
                | Ret

type Stack = [Value]
type Inst = [Instruction]
type Arg = [Value]

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
            | y == BoolVal False -> Left "Error: division by zero"
            | y /= 0 -> Right (Stack.push stack2 (x / y))
            | otherwise -> Left "Error: division by zero"
        (Nothing, _) -> Left "Error : Divide need two arguments"
    (Nothing, _) -> Left "Error : Divide need two arguments"
makeOperation Eq stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x == y -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : Equality need two arguments"
    (Nothing, _) -> Left "Error : Equality need two arguments"
makeOperation Less stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x < y -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : Less need two arguments"
    (Nothing, _) -> Left "Error : Less need two arguments"

isBoolVal :: Maybe Value -> Bool
isBoolVal (Just (BoolVal _)) = True
isBoolVal _ = False

exec :: Inst -> Stack -> Either String Value
exec (Call _:_) [] = Left "Error: stack is empty"
exec (Call op:xs) stack = case makeOperation op stack of
    Left err -> Left err
    Right newstack -> exec xs newstack
exec (Push val:xs) stack = exec xs (Stack.push stack val)
exec (JumpIfFalse val:xs) stack
  | null xs = Left "Error: no jump possible"
  | null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal True = exec xs stack
  | otherwise = exec (drop val xs) stack
exec (JumpIfTrue val:xs) stack
  | null xs = Left "Error: no jump possible"
  | null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal False = exec xs stack
  | otherwise = exec (drop val xs) stack
exec (Ret : _) stack = case Stack.top stack of
    Just x -> Right x
    Nothing -> Left "Error: stack is empty"
exec [] _ = Left "list no instruction found"
