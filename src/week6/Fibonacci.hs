module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 100 (streamToList s)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler = interleaveStreams (streamRepeat 0)
        (interleaveStreams (streamRepeat 1)
        (interleaveStreams (streamRepeat 2)
        (interleaveStreams (streamRepeat 3)
        (interleaveStreams (streamRepeat 4) (streamFromSeed (+1) 5)))))

zipStreams :: Stream a -> Stream b -> [(a, b)]
zipStreams s t = zip (streamToList s) (streamToList t)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s) (Cons y t) = Cons x (Cons y (interleaveStreams s t))
