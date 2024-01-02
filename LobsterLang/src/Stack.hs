{-
-- EPITECH PROJECT, 2023
-- GLaDOS
-- File description:
-- Stack
-}

module Stack(top,
             push,
             pop,
             seek,
             size,
             clear) where

-- | Return the top of the stack
top :: [a] -> Maybe a
top [] = Nothing
top (x:_) = Just x

-- | Push a value onto the stack, return the new stack
push :: [a] -> a -> [a]
push [] el = [el]
push l el = el:l

-- | Remove the top of the stack, return a tuple containing
-- the old top and the new stack
pop :: [a] -> (Maybe a, [a])
pop [] = (Nothing, [])
pop (x:l) = (Just x, l)

-- | Return the first element of the stack validating the
-- function given as parameter
seek :: (a -> Bool) -> [a] -> Maybe a
seek _ [] = Nothing
seek f (x:l) | f x = Just x
             | otherwise = seek f l

-- | Return the size of the stack
size :: [a] -> Int
size [] = 0
size (_:xs) = size xs + 1

-- | Clear the stack by return an empty stack
clear :: [a] -> [a]
clear _ = []
