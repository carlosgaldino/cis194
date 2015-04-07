{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.List (transpose, intercalate)

skips :: [a] -> [[a]]
skips xs = map (\n -> skip n xs) idxs
  where skip n ys = map snd . filter (\(i, _) -> i `rem` n == 0) $ zip idxs ys
        idxs = [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, x, _) -> x) . filter (\(x, y, z) -> y > x && y > z) $ zip3 xs (tail xs) (drop 2 xs)

histogram :: [Int] -> String
histogram xs = intercalate "\n" $ hist xs ++ [base]
  where max  = maximum $ frequences xs
        hist = transpose . map (complete max) . points . frequences

frequences :: [Int] -> [Int]
frequences xs = [freq | x <- [0..9], let freq = length $ filter (== x) xs]

points :: [Int] -> [String]
points = map (\x -> concat $ replicate x "*")

complete :: Int -> String -> String
complete n xs = concat (replicate (n - length xs) " ") ++ xs

base :: String
base = concat $ delimiter ++ ["\n"] ++ numbers
  where delimiter = replicate 10 "="
        numbers   = map show ([0..9] :: [Int])
