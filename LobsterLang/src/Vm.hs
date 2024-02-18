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
           | ListVal [Value]
           | Op Operator
           | Function Func Int
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
  _ + _  = IntVal 0
  (IntVal x) - (IntVal y) = IntVal (x - y)
  (BoolVal x) - (IntVal y) = IntVal (fromEnum x - y)
  (IntVal x) - (BoolVal y) = IntVal (x - fromEnum y)
  (BoolVal x) - (BoolVal y) = IntVal (fromEnum x - fromEnum y)
  (CharVal x) - (CharVal y) = IntVal (ord x - ord y)
  (CharVal x) - (IntVal y) = IntVal (ord x - y)
  (IntVal x) - (CharVal y) = IntVal (x - ord y)
  (BoolVal x) - (CharVal y) = IntVal (fromEnum x - ord y)
  (CharVal x) - (BoolVal y) = IntVal (ord x - fromEnum y)
  _ - _  = IntVal 0
  (IntVal x) * (IntVal y) = IntVal (x * y)
  (BoolVal x) * (IntVal y) = IntVal (fromEnum x * y)
  (IntVal x) * (BoolVal y) = IntVal (x * fromEnum y)
  (BoolVal x) * (BoolVal y) = IntVal (fromEnum x * fromEnum y)
  (CharVal x) * (CharVal y) = IntVal (ord x * ord y)
  (CharVal x) * (IntVal y) = IntVal (ord x * y)
  (IntVal x) * (CharVal y) = IntVal (x * ord y)
  (BoolVal x) * (CharVal y) = IntVal (fromEnum x * ord y)
  (CharVal x) * (BoolVal y) = IntVal (ord x * fromEnum y)
  _ * _  = IntVal 0
  abs (IntVal x) = IntVal (abs x)
  abs (BoolVal x) = IntVal (abs (fromEnum x))
  abs (CharVal x) = IntVal (abs (ord x))
  abs _ = IntVal 0
  signum (IntVal x) = IntVal (signum x)
  signum (BoolVal x) = IntVal (signum (fromEnum x))
  signum (CharVal x) = IntVal (signum (ord x))
  signum _ = IntVal 0
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
  _ / _ = IntVal 0
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
              | Append
              | RmOcc
              | Len

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
    show RmOcc = "--"
    show Append = "++"
    show Len = "~"

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
    Len == Len = True
    _ == _ = False

data Instruction = Push Value
                | PushArg Int
                | PushEnv String
                | PutArg
                | Call
                | JumpIfFalse Int
                | JumpIfTrue Int
                | Jump Int
                | Define String
                | PushList Int
                | Ret

instance Show Instruction where
    show (Push val) = "Push " ++ show val
    show (PushArg x) = "PushArg " ++ show x
    show (PushEnv x) = "PushEnv " ++ show x
    show PutArg = "PutArg"
    show Call = "Call"
    show (JumpIfFalse x) = "JumpIfFalse " ++ show x
    show (JumpIfTrue x) = "JumpIfTrue " ++ show x
    show (Jump x) = "Jump " ++ show x
    show (Define x) = "Define " ++ show x
    show Ret = "Ret"
    show (PushList x) = "PushList " ++ show x

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
type Env = [(String, Value, Int)]

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
            (IntVal a, IntVal b) ->
                Right (Stack.push stack2 (IntVal (a `mod` b)))
            _ -> Left "Error: Mod needs two integer arguments"
        (Nothing, _) -> Left "Error : Mod need two arguments"
    (Nothing, _) -> Left "Error : Mod need two arguments"
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
        (Just y, stack2) -> Right (Stack.push stack2
            (BoolVal (x == BoolVal True && y == BoolVal True)))
        (Nothing, _) -> Left "Error : And need two arguments"
    (Nothing, _) -> Left "Error : And need two arguments"
makeOperation Or stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2
            (BoolVal (x == BoolVal True || y == BoolVal True)))
        (Nothing, _) -> Left "Error : Or need two arguments"
    (Nothing, _) -> Left "Error : Or need two arguments"
makeOperation Xorb stack = case Stack.pop stack of
    (Just x, stack1) -> case Stack.pop stack1 of
        (Just y, stack2) -> Right (Stack.push stack2 (BoolVal
            ((x == BoolVal True && y == BoolVal False) || (x == BoolVal False
            && y == BoolVal True))))
        (Nothing, _) -> Left "Error : XOrb need two arguments"
    (Nothing, _) -> Left "Error : XOrb need two arguments"
makeOperation Not stack = case Stack.pop stack of
    (Just x, stack1) ->
        Right (Stack.push stack1 (BoolVal (x == BoolVal False)))
    (Nothing, _) -> Left "Error : Not need One arguments"
makeOperation ToString stack = case Stack.pop stack of
    (Just (IntVal x), stack1) -> Right (Stack.push stack1 (StringVal (show x)))
    (Just (BoolVal x), stack1) ->
        Right (Stack.push stack1 (StringVal (show x)))
    (Just (CharVal x), stack1) ->
        Right (Stack.push stack1 (StringVal (show x)))
    (Just (StringVal x), stack1) -> Right (Stack.push stack1 (StringVal x))
    (Just _, _) -> Left "Error : Cannot convert to string"
    (Nothing, _) -> Left "Error : ToString need One arguments"
makeOperation Get stack = makeOperationGet (Stack.pop stack)
makeOperation Append stack = case Stack.pop stack of
    (Just (ListVal l), stack1) -> case Stack.pop stack1 of
        (Just v, stack2) -> Right (Stack.push stack2 (ListVal (l ++ [v])))
        (Nothing, _) -> Left "Error : Append need two arguments"
    (Just _, _) -> Left "Error : Cannot Append on not a List"
    (Nothing, _) -> Left "Error : Append need two arguments"
makeOperation RmOcc stack = case Stack.pop stack of
    (Just (ListVal l), stack1) -> case Stack.pop stack1 of
        (Just v, stack2) -> Right (Stack.push stack2
            (ListVal (filter (/= v) l)))
        (Nothing, _) -> Left "Error : RmOcc need two arguments"
    (Just _, _) -> Left "Error : Cannot RmOcc on not a List"
    (Nothing, _) -> Left "Error : RmOcc need two arguments"
makeOperation Len stack = case Stack.pop stack of
    (Just (StringVal s), stack1) ->
        Right (Stack.push stack1 (IntVal (length s)))
    (Just (ListVal l), stack1) -> Right (Stack.push stack1 (IntVal (length l)))
    (Just _, _) -> Left "Error : Len no len"
    (Nothing, _) -> Left "Error : Len need one arguments"

makeOperationGet :: (Maybe Value, Stack) -> Either String Stack
makeOperationGet (Just (StringVal s), stack1) = case Stack.pop stack1 of
    (Just (IntVal x), stack2) ->
        Right (Stack.push stack2 (StringVal [s !! x]))
    (Just _, _) -> Left "Error : Wrong arguments for Get"
    (Nothing, _) -> Left "Error : Get need two arguments"
makeOperationGet (Just (ListVal l), stack1) = case Stack.pop stack1 of
    (Just (IntVal x), stack2) -> Right (Stack.push stack2 (l !! x))
    (Just _, _) -> Left "Error : Wrong arguments for Get"
    (Nothing, _) -> Left "Error : Get need two arguments"
makeOperationGet (Just _, _) =
    Left "Error : Cannot Get on not a String nor List"
makeOperationGet (Nothing, _) = Left "Error : Get need two arguments"

isBoolVal :: Maybe Value -> Bool
isBoolVal (Just (BoolVal _)) = True
isBoolVal _ = False

isInEnv :: String -> Int -> Env -> Maybe Value
isInEnv _ _ [] = Nothing
isInEnv s d ((name, val, depth):as)
    | name == s, depth `elem` [0, d] = Just val
    | otherwise = isInEnv s d as

updateInEnv :: String -> Int -> Value -> Env -> Env
updateInEnv _ _ _ [] = []
updateInEnv s d nv ((name, val, depth):as)
    | name == s, depth `elem` [0, d] = (name, nv, depth) : as
    | otherwise = (name, val, depth) : updateInEnv s d nv as

clearUntilDepth :: Env -> Int -> Env
clearUntilDepth [] _ = []
clearUntilDepth ((name, val, depth):as) d
    | depth > d = clearUntilDepth as d
    | otherwise = (name, val, depth):as

createList :: Int -> Stack -> [Value] -> (Stack, [Value])
createList 0 stack val = (stack, val)
createList n stack val = case Stack.pop stack of
    (Nothing, _) -> (stack, val)
    (Just x, stack1) -> createList (n - 1) stack1 (val ++ [x])

exec :: Int -> Env -> Arg -> Inst -> Stack -> (Either String Value, Env)
exec _ _ _ (Call : _) [] = (Left "Error: stack is empty", [])
exec depth env arg (Call : xs) stack =
    execCall depth env arg xs stack (Stack.pop stack)
exec _ _ [] (PushArg _:_) _ = (Left "Error: no Arg", [])
exec depth env arg (PushArg x:xs) stack
    | x < 0 = (Left "Error index out of range", env)
    | x >= length arg = (Left "Error: index out of range", env)
    | otherwise = exec depth env arg xs (Stack.push stack (arg !! x))
exec depth env arg (PushList x:xs) stack
    | x < 0 = (Left "Error: index out of range", env)
    | x > length stack = (Left "Error: index out of range", env)
    | otherwise = exec depth env arg xs (ListVal (snd (createList x stack []))
        : fst (createList x stack []))
exec _ [] _ (PushEnv _:_) _ = (Left "Error: no Env", [])
exec depth env arg (PushEnv x:xs) stack =  case isInEnv x depth env of
    Nothing -> (Left ("Error: not in environment " ++ x ++ " " ++ show depth),
        env)
    Just (BoolVal b) -> exec depth env arg  (Push (BoolVal b):xs) stack
    Just (IntVal i) -> exec depth env arg  (Push (IntVal i):xs) stack
    Just (CharVal c) -> exec depth env arg  (Push (CharVal c):xs) stack
    Just (StringVal str) -> exec depth env arg  (Push (StringVal str):xs) stack
    Just (Op op) -> exec depth env arg (Push (Op op):xs) stack
    Just (Function f nb) -> exec depth env arg (Push (Function f nb):xs) stack
    Just (ListVal list) -> exec depth env arg (Push (ListVal list):xs) stack
exec depth env arg (Push val:xs) stack =
    exec depth env arg xs (Stack.push stack val)
exec depth env arg (PutArg:xs) stack = case Stack.pop stack of
    (Nothing, _) -> (Left "Error: stack is empty", env)
    (Just val, stack1) -> exec depth env (arg ++ [val]) xs stack1
exec depth env arg (JumpIfFalse val:xs) stack
  | Prelude.null xs = (Left "Error: no jump possible", env)
  | Prelude.null stack = (Left "Error: stack is empty", env)
  | val < 0 = (Left "Error: invalid jump value", env)
  | val > length xs = (Left "Error: invalid jump value", env)
  | not (isBoolVal (Stack.top stack)) = (Left "Error: not bool", env)
  | head stack == BoolVal True = exec depth env arg xs stack
  | otherwise = exec depth env arg (Prelude.drop val xs) stack
exec depth env arg (JumpIfTrue val:xs) stack
  | Prelude.null xs = (Left "Error: no jump possible", env)
  | Prelude.null stack = (Left "Error: stack is empty", env)
  | val < 0 = (Left "Error: invalid jump value", env)
  | val > length xs = (Left "Error: invalid jump value", env)
  | not (isBoolVal (Stack.top stack)) = (Left "Error: not bool", env)
  | head stack == BoolVal False = exec depth env arg xs stack
  | otherwise = exec depth env arg (Prelude.drop val xs) stack
exec depth env arg (Jump val:xs) stack
  | Prelude.null xs = (Left "Error: no jump possible", env)
  | val < 0 = (Left "Error: invalid jump value", env)
  | val > length xs = (Left "Error: invalid jump value", env)
  | otherwise = exec depth env arg (Prelude.drop val xs) stack
exec depth env arg (Define str:xs) stack = case Stack.pop stack of
    (Nothing, _) -> (Left "Error: stack is empty", env)
    (Just val, stack1) -> case isInEnv str depth env of
        Nothing -> exec depth ((str, val, depth):env) arg xs stack1
        _ -> exec depth (updateInEnv str depth val env) arg xs stack1
exec _ env _ (Ret : _) stack = case Stack.top stack of
    Just x -> (Right x, env)
    Nothing -> (Left "Error: stack is empty", env)
exec _ _ _ [] _ = (Left "list no instruction found", [])

execCall :: Int -> Env -> Arg -> Inst -> Stack ->
    (Maybe Value, Stack) -> (Either String Value, Env)
execCall _ env _ _ _ (Nothing, _) =
    (Left "Error: stack is empty", env)
execCall d env arg xs _ (Just (Op x), s1) =
    case makeOperation x s1 of
       Left err -> (Left err, env)
       Right news -> exec d env arg xs news
execCall d env arg xs _ (Just (Function body 0), s1) =
    case exec (d + 1) env [] body [] of
        (Left err, _) -> (Left err, env)
        (Right val, env') -> exec d (clearUntilDepth env' d)
            arg xs (Stack.push s1 val)
execCall d env arg xs _ (Just (Function body nb), s1) = case Stack.pop s1 of
    (Just (IntVal 0), s2) -> exec d env arg xs
        (Stack.push s2 (Function body nb))
    (Just (IntVal nb'), s2)
      | nb < nb' -> (Left "Error: too much arguments given", env)
      | otherwise -> case Stack.pop s2 of
        (Just v, s3) -> exec d env arg (Call:xs) (Stack.push (Stack.push
          s3 (IntVal (nb' - 1))) (Function (Push v:PutArg:body) (nb - 1)))
        (Nothing, _) -> (Left "Error: stack is empty", env)
    (_, _) -> (Left "Error: stack is invalid for a function call", env)
execCall _ env _ _ s  (Just a, _) =
    (Left ("Error: not an Operation or a function " ++
    show a ++ "stack : " ++ show s), env)
