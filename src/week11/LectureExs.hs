module LectureExs where

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
f *> g = liftA2 (const id) f g

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = \xs -> sequenceA $ f <$> xs

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
-- or
-- sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA x fa = replicate x <$> fa
