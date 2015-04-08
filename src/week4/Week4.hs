{-# OPTIONS_GHC -Wall #-}

module Week4 where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

xor :: [Bool] -> Bool
xor = foldl (\x y -> not (x == y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where step x g a = g (f a x)
