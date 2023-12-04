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

top :: [a] -> Maybe a
top [] = Nothing
top (x:_) = Just x

push :: [a] -> a -> [a]
push [] el = [el]
push l el = (el:l)

pop :: [a] -> (Maybe a, [a])
pop [] = (Nothing, [])
pop (x:l) = (Just x, l)

seek :: (a -> Bool) -> [a] -> Maybe a
seek _ [] = Nothing
seek f (x:l) | f x = Just x
             | otherwise = seek f l

size :: [a] -> Int
size [] = 0
size (_:xs) = size xs + 1

clear :: [a] -> [a]
clear _ = []
