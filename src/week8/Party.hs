{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0

  mappend (GL es _) h = foldr glCons h es

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z t = foldl f z (nodeList t)

nodeList :: Tree a -> [a]
nodeList (Node a as) = a : concatMap nodeList as
