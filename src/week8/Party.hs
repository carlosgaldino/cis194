{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0

  mappend (GL es _) h = foldr glCons h es

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z t = foldl f z (nodeList t)
  where nodeList (Node a as) = a : concatMap nodeList as

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (maximum (map fst maxis), maximum (map snd maxis))
  where maxis = map (\(x, y) -> (moreFun (glCons e x) (glCons e y), moreFun x y)) gs

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold (\gs e -> nextLevel e [gs]) emptyGL t
  where emptyGL = (mempty, mempty)

printFun :: GuestList -> IO ()
printFun (GL _ f) = putStrLn ("Total fun: " ++ show f)

printGuests :: GuestList -> IO ()
printGuests (GL gs _) = mapM_ print $ sort (map empName gs)

main :: IO ()
main = do
  content <- readFile "company.txt"
  let guestList = maxFun (read content :: Tree Employee)
  printFun guestList
  printGuests guestList
