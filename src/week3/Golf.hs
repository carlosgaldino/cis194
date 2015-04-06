module Golf where

skips :: [a] -> [[a]]
skips xs = map (\n -> skip n xs) idxs
  where skip n xs = map snd . filter (\(i, _) -> i `rem` n == 0) $ zip idxs xs
        idxs = [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, x, _) -> x) . filter (\(x, y, z) -> y > x && y > z) $ zip3 xs (tail xs) (drop 2 xs)

histogram :: [Integer] -> String
histogram = undefined

frequences :: [Integer] -> [(Int, Integer)]
frequences xs = [(length fs, x) | x <- [0..9], let fs = filter (== x) xs]
