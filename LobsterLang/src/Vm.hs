{-
-- EPITECH PROJECT, 2023
-- LobsterLang
-- File description:
-- vm
-}

module Vm (Operator(..),
           Value(..),
           Instruction(..),
           Stack,
           Inst,
           Arg,
           Func,
           Env,
           makeOperation,
           exec) where

import Stack
import Data.Ratio
import Data.Char

data Value = IntVal Int
           | BoolVal Bool
           | CharVal Char
           | StringVal String
        --    | ListVal [Value]
           | Op Operator
           | Function Func
           deriving (Show, Eq, Ord)

instance Num Value where
  (BoolVal x) + (IntVal y) = IntVal (fromEnum x + y)
  (IntVal x) + (BoolVal y) = IntVal (x + fromEnum y)
  (IntVal x) + (IntVal y) = IntVal (x + y)
  (BoolVal x) + (BoolVal y) = IntVal (fromEnum x + fromEnum y)
  (CharVal x) + (CharVal y) = IntVal (ord x + ord y)
  (CharVal x) + (IntVal y) = IntVal (ord x + y)
  (IntVal x) + (CharVal y) = IntVal (x + ord y)
  (BoolVal x) + (CharVal y) = IntVal (fromEnum x + ord y)
  (CharVal x) + (BoolVal y) = IntVal (ord x + fromEnum y)
  _ + _  = IntVal (0)
  (IntVal x) - (IntVal y) = IntVal (x - y)
  (BoolVal x) - (IntVal y) = IntVal (fromEnum x - y)
  (IntVal x) - (BoolVal y) = IntVal (x - fromEnum y)
  (BoolVal x) - (BoolVal y) = IntVal (fromEnum x - fromEnum y)
  (CharVal x) - (CharVal y) = IntVal (ord x - ord y)
  (CharVal x) - (IntVal y) = IntVal (ord x - y)
  (IntVal x) - (CharVal y) = IntVal (x - ord y)
  (BoolVal x) - (CharVal y) = IntVal (fromEnum x - ord y)
  (CharVal x) - (BoolVal y) = IntVal (ord x - fromEnum y)
  _ - _  = IntVal (0)
  (IntVal x) * (IntVal y) = IntVal (x * y)
  (BoolVal x) * (IntVal y) = IntVal (fromEnum x * y)
  (IntVal x) * (BoolVal y) = IntVal (x * fromEnum y)
  (BoolVal x) * (BoolVal y) = IntVal (fromEnum x * fromEnum y)
  (CharVal x) * (CharVal y) = IntVal (ord x * ord y)
  (CharVal x) * (IntVal y) = IntVal (ord x * y)
  (IntVal x) * (CharVal y) = IntVal (x * ord y)
  (BoolVal x) * (CharVal y) = IntVal (fromEnum x * ord y)
  (CharVal x) * (BoolVal y) = IntVal (ord x * fromEnum y)
  _ * _  = IntVal (0)
  abs (IntVal x) = IntVal (abs x)
  abs (BoolVal x) = IntVal (abs (fromEnum x))
  abs (CharVal x) = IntVal (abs (ord x))
  abs _ = IntVal (0)
  signum (IntVal x) = IntVal (signum x)
  signum (BoolVal x) = IntVal (signum (fromEnum x))
  signum (CharVal x) = IntVal (signum (ord x))
  signum _ = IntVal (0)
  fromInteger x = IntVal (fromInteger x)

instance Fractional Value where
  (IntVal x) / (IntVal y) = IntVal (x `div` y)
  (BoolVal x) / (IntVal y) = IntVal (fromEnum x `div` y)
  (IntVal x) / (BoolVal y) = IntVal (x `div` fromEnum y)
  (BoolVal x) / (BoolVal y) = IntVal (fromEnum x `div` fromEnum y)
  (CharVal x) / (CharVal y) = IntVal (ord x `div` ord y)
  (CharVal x) / (IntVal y) = IntVal (ord x `div` y)
  (IntVal x) / (CharVal y) = IntVal (x `div` ord y)
  (CharVal x) / (BoolVal y) = IntVal (ord x `div` fromEnum y)
  (BoolVal x) / (CharVal y) = IntVal (fromEnum x `div` ord y)
  _ / _ = IntVal (0)
  fromRational x = IntVal (fromInteger (numerator x) `div` fromInteger (denominator x))

data Operator = Add
              | Sub
              | Mul
              | Div
              | Mod
              | Eq
              | Less
              | LessEq
              | Great
              | GreatEq
              | And
              | Or
              | Xorb
              | Not
              | ToString
              | Get

instance Ord Operator where
    compare op1 op2 = compare (show op1) (show op2)

instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Eq = "=="
    show Less = "<"
    show LessEq = "<="
    show Great = ">"
    show GreatEq = ">="
    show And = "&&"
    show Or = "||"
    show Xorb = "^^"
    show Not = "!"
    show ToString = "@"
    show Get = "!!"

instance Eq Operator where
    Add == Add = True
    Sub == Sub = True
    Mul == Mul = True
    Div == Div = True
    Eq == Eq = True
    Mod == Mod = True
    Less == Less = True
    Great == Great = True
    LessEq == LessEq = True
    GreatEq == GreatEq = True
    And == And = True
    Or == Or = True
    Xorb == Xorb = True
    Not == Not = True
    _ == _ = False

data Instruction = Push Value
                | PushArg Int
                | PushEnv String
                | Call
                | JumpIfFalse Int
                | JumpIfTrue Int
                | Jump Int
                | Ret

instance Show Instruction where
    show (Push val) = "Push " ++ show val
    show (PushArg x) = "PushArg " ++ show x
    show (PushEnv x) = "PushEnv " ++ show x
    show Call = "Call"
    show (JumpIfFalse x) = "JumpIfFalse " ++ show x
    show (JumpIfTrue x) = "JumpIfTrue " ++ show x
    show (Jump x) = "Jump " ++ show x
    show Ret = "Ret"

instance Ord Instruction where
    compare inst1 inst2 = compare (show inst1) (show inst2)

instance Eq Instruction where
    (Push _) == (Push _) = True
    (PushArg _) == (PushArg _) = True
    (PushEnv _) == (PushEnv _) = True
    Call == Call = True
    (JumpIfFalse _) == (JumpIfFalse _) = True
    (JumpIfTrue _) == (JumpIfTrue _) = True
    Ret == Ret = True
    _ == _ = False

type Stack = [Value]
type Inst = [Instruction]
type Arg = [Value]
type Func = [Instruction]
type Env = [(String, Value)]

makeOperation :: Operator -> Stack -> Either String Stack
makeOperation Add stack = case Stack.pop stack of
    (Nothing, _) -> Left "Error : Add need two arguments"
    (Just (StringVal s), stack1) -> case Stack.pop stack1 of
        (Just (StringVal xs), stack2) ->
            Right (Stack.push stack2 (StringVal (s ++ xs)))
        (Just _, _) -> Left "Error : invalide operation on string"
        (Nothing, _) -> Left "Error : Add need two arguments"
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x + y))
        (Nothing, _) -> Left "Error : Add need two arguments"
makeOperation Sub stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x - y))
        (Nothing, _) -> Left "Error : Sub need two arguments"
    (Nothing, _) -> Left "Error : Sub need two arguments"
makeOperation Mul stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (x * y))
        (Nothing, _) -> Left "Error : Mul need two arguments"
    (Nothing, _) -> Left "Error : Mul need two arguments"
makeOperation Div stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just (BoolVal False), _) -> Left "Error: division by zero"
        (Just (IntVal 0), _) -> Left "Error: division by zero"
        (Just y, stack2) -> Right (Stack.push stack2 (x / y))
        (Nothing, _) -> Left "Error : Div need two arguments"
    (Nothing, _) -> Left "Error : Div need two arguments"
makeOperation Mod stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> case (x, y) of
            (IntVal a, IntVal b) -> Right (Stack.push stack2 (IntVal (a `mod` b)))
            _ -> Left "Error: Mod needs two integer arguments"
        (Nothing, _) -> Left "Error : Div need two arguments"
    (Nothing, _) -> Left "Error : Div need two arguments"
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
makeOperation LessEq stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x <= y -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : LessEq need two arguments"
    (Nothing, _) -> Left "Error : LessEq need two arguments"
makeOperation Great stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x > y -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : Great need two arguments"
    (Nothing, _) -> Left "Error : Great need two arguments"
makeOperation GreatEq stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x >= y -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : GreatEq need two arguments"
    (Nothing, _) -> Left "Error : GreatEq need two arguments"
makeOperation And stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x == (BoolVal True) && y == (BoolVal True) -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : Great need two arguments"
    (Nothing, _) -> Left "Error : Great need two arguments"
makeOperation Or stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x == (BoolVal True) || y == (BoolVal True) -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : Or need two arguments"
    (Nothing, _) -> Left "Error : Or need two arguments"
makeOperation Xorb stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2)
            | x == (BoolVal True) && y == (BoolVal True) -> Right (Stack.push stack2 (BoolVal True))
            | x == (BoolVal False) && y == (BoolVal False) -> Right (Stack.push stack2 (BoolVal True))
            | otherwise -> Right (Stack.push stack2 (BoolVal False))
        (Nothing, _) -> Left "Error : XOrb need two arguments"
    (Nothing, _) -> Left "Error : XOrb need two arguments"
makeOperation Not stack = case Stack.pop stack of
    (Just x, stack1)
        | x == (BoolVal False) -> Right (Stack.push stack1 (BoolVal True))
        | otherwise -> Right (Stack.push stack1 (BoolVal False))
    (Nothing, _) -> Left "Error : Great need One arguments"
makeOperation ToString stack = case Stack.pop stack of
    (Just (IntVal x), stack1) -> Right (Stack.push stack1 (StringVal (show x)))
    (Just (BoolVal x), stack1) -> Right (Stack.push stack1 (StringVal (show x)))
    (Just (CharVal x), stack1) -> Right (Stack.push stack1 (StringVal (show x)))
    (Just (StringVal x), stack1) -> Right (Stack.push stack1 (StringVal x))
    (Just _, _) -> Left "Error : Cannot convert to string"
    (Nothing, _) -> Left "Error : ToString need One arguments"
makeOperation Get stack = case Stack.pop stack of
    (Just (StringVal s), stack1) -> case Stack.pop stack1 of
        (Just (IntVal x), stack2) -> Right (Stack.push stack2 (StringVal [s !! x]))
        (Just _, _) -> Left "Error : Wrong arguments for Get"
        (Nothing, _) -> Left "Error : Get need two arguments"
    (Just _, _) -> Left "Error : Cannot Get on not a String nor List"
    (Nothing, _) -> Left "Error : Get need two arguments"

isBoolVal :: Maybe Value -> Bool
isBoolVal (Just (BoolVal _)) = True
isBoolVal _ = False

isInEnv :: String -> Env -> Maybe Value
isInEnv _ [] = Nothing
isInEnv s (xs:as)
    | fst xs == s = Just (snd xs)
    | fst xs /= s = isInEnv s as
isInEnv _ _ = Nothing

exec :: Env -> Arg -> Inst -> Stack -> Either String Value
exec _ _ (Call : _) [] = Left "Error: stack is empty"
exec env arg (Call : xs) stack = case Stack.pop stack of
        (Nothing, _) -> Left "Error: stack is empty"
        (Just (Op x), stack1)  -> case makeOperation x stack1 of
               Left err -> Left err
               Right newstack -> exec env arg xs newstack
        (Just (Function x), stack1) -> case exec env stack1 x [] of
                Left err -> Left err
                Right val -> exec env arg xs (Stack.push stack1 val)
        (Just a, _) -> Left ("Error: not an Operation or a function" ++ show a)
exec _ [] (PushArg _:_) _ = Left "Error: no Arg"
exec env arg (PushArg x:xs) stack
    | x < 0 = Left "Error index out of range"
    | x >= length arg = Left "Error: index out of range"
    | otherwise = exec env arg xs (Stack.push stack (arg !! x))
exec [] _ (PushEnv _:_) _ = Left "Error: no Env"
exec env arg (PushEnv x:xs) stack =  case isInEnv x env of
    Nothing -> Left "Error: not in environment"
    Just (BoolVal b) -> exec env arg  (Push (BoolVal b):xs) stack
    Just (IntVal i) -> exec env arg  (Push (IntVal i):xs) stack
    Just (CharVal c) -> exec env arg  (Push (CharVal c):xs) stack
    Just (StringVal str) -> exec env arg  (Push (StringVal str):xs) stack
    Just (Op op) -> exec env arg (Push (Op op):xs) stack
    Just (Function func) -> exec env arg (Push (Function func):xs) stack
exec env arg (Push val:xs) stack = exec env arg xs (Stack.push stack val)
exec env arg (JumpIfFalse val:xs) stack
  | Prelude.null xs = Left "Error: no jump possible"
  | Prelude.null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal True = exec env arg xs stack
  | otherwise = exec env arg (Prelude.drop val xs) stack
exec env arg (JumpIfTrue val:xs) stack
  | Prelude.null xs = Left "Error: no jump possible"
  | Prelude.null stack = Left "Error: stack is empty"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | not (isBoolVal (Stack.top stack)) = Left "Error: not bool"
  | (head stack) == BoolVal False = exec env arg xs stack
  | otherwise = exec env arg (Prelude.drop val xs) stack
exec env arg (Jump val:xs) stack
  | Prelude.null xs = Left "Error: no jump possible"
  | val < 0 = Left "Error: invalid jump value"
  | val > length xs = Left "Error: invalid jump value"
  | otherwise = exec env arg (Prelude.drop val xs) stack
exec _ _ (Ret : _) stack = case Stack.top stack of
    Just x -> Right x
    Nothing -> Left "Error: stack is empty"
exec _ _ [] _ = Left "list no instruction found"
