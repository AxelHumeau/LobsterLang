{-
-- EPITECH PROJECT, 2024
-- GLaDOS
-- File description:
-- Utils
-}

module Utils (
    applyNTimes,
) where

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes _ 0 el = el
applyNTimes f n el = f (applyNTimes f (n - 1) el)
