{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.List (sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving (Show)

maxUnits :: Battlefield -> (Army, Army)
maxUnits b = (min 3 (attackers b - 1), min 2 (defenders b))

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

data Victory = Attack | Defense
             deriving (Eq, Ord)

victory :: DieValue -> DieValue -> Victory
victory x y = if x > y then Attack else Defense

matchUp :: [DieValue] -> [DieValue] -> [Victory]
matchUp as ds = zipWith victory (sortBy (flip compare) as) (sortBy (flip compare) ds)

victories :: Victory -> [Victory] -> Int
victories v = foldr (\x a -> if x == v then a + 1 else a) 0

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attacks <- dice attackingUnits
  defenses <- dice defendingUnits
  let match = matchUp attacks defenses
  let attv = victories Attack match
  let defv = victories Defense match
  return Battlefield { attackers = attackers b - defv, defenders = defenders b - attv }
    where attackingUnits = fst units
          defendingUnits = snd units
          units = maxUnits b

main :: IO ()
main = do
  att <- readLn :: IO Int
  def <- readLn :: IO Int
  b <- evalRandIO $ battle Battlefield { attackers = att, defenders = def }
  print b
