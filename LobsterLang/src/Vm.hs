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
           | Op Operator
           | Function Func
           deriving (Show, Eq, Ord)

instance Num Value where
  (BoolVal x) + (IntVal y) = IntVal (fromEnum x + y)
  (IntVal x) + (BoolVal y) = IntVal (x + fromEnum y)
  (IntVal x) + (IntVal y) = IntVal (x + y)
  (BoolVal x) + (BoolVal y) = IntVal (fromEnum x + fromEnum y)
  _ + _  = IntVal (0)
  (IntVal x) - (IntVal y) = IntVal (x - y)
  (BoolVal x) - (IntVal y) = IntVal (fromEnum x - y)
  (IntVal x) - (BoolVal y) = IntVal (x - fromEnum y)
  (BoolVal x) - (BoolVal y) = IntVal (fromEnum x - fromEnum y)
  _ - _  = IntVal (0)
  (IntVal x) * (IntVal y) = IntVal (x * y)
  (BoolVal x) * (IntVal y) = IntVal (fromEnum x * y)
  (IntVal x) * (BoolVal y) = IntVal (x * fromEnum y)
  (BoolVal x) * (BoolVal y) = IntVal (fromEnum x * fromEnum y)
  _ * _  = IntVal (0)
  abs (IntVal x) = IntVal (abs x)
  abs (BoolVal x) = IntVal (abs (fromEnum x))
  abs _ = IntVal (0)
  signum (IntVal x) = IntVal (signum x)
  signum (BoolVal x) = IntVal (signum (fromEnum x))
  signum _ = IntVal (0)
  fromInteger x = IntVal (fromInteger x)

instance Fractional Value where
  (IntVal x) / (IntVal y) = IntVal (x `div` y)
  (BoolVal x) / (IntVal y) = IntVal (fromEnum x `div` y)
  (IntVal x) / (BoolVal y) = IntVal (x `div` fromEnum y)
  (BoolVal x) / (BoolVal y) = IntVal (fromEnum x `div` fromEnum y)
  _ / _ = IntVal (0)
  fromRational x = IntVal (fromInteger (numerator x) `div` fromInteger (denominator x))


data Operator = Add
              | Subtract
              | Multiply
              | Divide
              | Eq
              | Less

instance Ord Operator where
    compare op1 op2 = compare (show op1) (show op2)

instance Show Operator where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"
    show Eq = "=="
    show Less = "<"

instance Eq Operator where
    Add == Add = True
    Subtract == Subtract = True
    Multiply == Multiply = True
    Divide == Divide = True
    Eq == Eq = True
    Less == Less = True
    _ == _ = False

data Instruction = Push Value
                | PushArg Int
                | Call
                | JumpIfFalse Int
                | JumpIfTrue Int
                | Ret

instance Show Instruction where
    show (Push val) = "Push " ++ show val
    show (PushArg x) = "PushArg " ++ show x
    show Call = "Call"
    show (JumpIfFalse x) = "JumpIfFalse " ++ show x
    show (JumpIfTrue x) = "JumpIfTrue " ++ show x
    show Ret = "Ret"

instance Ord Instruction where
    compare inst1 inst2 = compare (show inst1) (show inst2)

instance Eq Instruction where
    (Push _) == (Push _) = True
    (PushArg _) == (PushArg _) = True
    Call == Call = True
    (JumpIfFalse _) == (JumpIfFalse _) = True
    (JumpIfTrue _) == (JumpIfTrue _) = True
    Ret == Ret = True
    _ == _ = False

type Stack = [Value]
type Inst = [Instruction]
type Arg = [Value]
type Func = [Instruction]

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

exec :: Arg -> Inst -> Stack -> Either String Value
exec _ (Call : _) [] = Left "Error: stack is empty"
exec arg (Call : xs) stack = case Stack.pop stack of
        (Nothing, _) -> Left "Error: stack is empty"
        (Just (Op x), stack1)  -> case makeOperation x stack1 of
               Left err -> Left err
               Right newstack -> exec arg xs newstack
        (Just (Function x), stack1) -> case exec stack1 x [] of
                Left err -> Left err
                Right val -> exec arg xs (Stack.push stack1 val)
        (Just _, _) -> Left "Error: not an Operation or a function"
exec [] (PushArg _:_) _ = Left "Error: no Arg"
exec arg (PushArg x:xs) stack
    | x < 0 = Left "Error index out of range"
    | x >= length arg = Left "Error: index out of range"
    | otherwise = exec arg xs (Stack.push stack (arg !! x))
exec arg (Push val:xs) stack = exec arg xs (Stack.push stack val)
exec arg (JumpIfFalse val:xs) stack
  | null xs = Left "Error: no jump possible"
  | null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal True = exec arg xs stack
  | otherwise = exec arg (drop val xs) stack
exec arg (JumpIfTrue val:xs) stack
  | null xs = Left "Error: no jump possible"
  | null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal False = exec arg xs stack
  | otherwise = exec arg (drop val xs) stack
exec _ (Ret : _) stack = case Stack.top stack of
    Just x -> Right x
    Nothing -> Left "Error: stack is empty"
exec _ [] _ = Left "list no instruction found"
