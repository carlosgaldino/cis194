{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append ((tag x) <> (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x _ _) = x

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

sz :: (Sized b, Monoid b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append m l r)
  | i >= getSize (size m) = Nothing
  | i < sz l = indexJ i l
  | otherwise = indexJ (i - sz l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append m l r)
  | i >= getSize (size m) = Empty
  | i >= sz l = dropJ (i - sz l) r
  | otherwise = (dropJ i l) +++ r

verifyDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

verifyTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl
  | i <= 0 = Empty
  | i >= sz jl = jl
takeJ i (Append m l r)
  | i < sz l = takeJ i l
  | otherwise = l +++ takeJ (i - sz l) r

scoreLine :: String -> JoinList Score String
scoreLine xs = Single (scoreString xs) xs

type JLBuffer = JoinList (Score, Size) String

instance Buffer JLBuffer where
  toString = unlines . jlToList

  fromString = foldr (+++) Empty . map (\s -> Single (scoreString s, 1) s) . lines

  line = indexJ

  replaceLine x s jl = takeJ x jl +++ fromString s +++ dropJ (x + 1) jl

  numLines = sz

  value = getScore . fst . tag
    where getScore (Score i) = i

main = runEditor editor jlb
  where jlb = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JLBuffer
