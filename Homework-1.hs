module Homework1 where

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
  | (length xs) `mod` 2 == 0 = [x] ++ doubleEveryOther xs
  | otherwise                = [2 * x] ++ doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate x = checkSum x `rem` 10 == 0
  where checkSum = sumDigits . doubleEveryOther . toDigits

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
