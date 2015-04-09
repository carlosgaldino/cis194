{-# LANGUAGE FlexibleInstances #-}

module Exercise5 where

import StackVM
import Parser

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr Program where
  lit x = [PushI x]
  mul x y = x ++ y ++ [Mul]
  add x y = x ++ y ++ [Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul
