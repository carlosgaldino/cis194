{-# OPTIONS_GHC -Wall #-}

module Week4 where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l v r)
  | height l <= height r = Node (maxHeight (newNode l) r + 1) (newNode l) v r
  | otherwise = Node (maxHeight l (newNode r) + 1) l v (newNode r)
  where maxHeight l r = max (height l) (height r)
        newNode t = insert x t

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl (\x y -> not (x == y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where step x g a = g (f a x)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2:[2 * x + 1 | x <- [1..n], not (x `elem` excludingList n)]
  where excludingList n = [i + j + 2 * i * j | i <- [1..n], j <- [i..maxJ i]]
        maxJ i = (n - i) `div` (1 + 2 * i)
