{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort, sortBy)
import System.Posix.Signals.Exts (windowChange)

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

type Army    = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = 
  liftRand $ \g -> 
    let 
      aCount = attackers bf

      dCount = defenders bf

      calcARolls n 
                  | n <=  0 = 0
                  | n ==  1 = 1
                  | n ==  2 = 1
                  | n ==  3 = 2
                  | n >=  4 = 3
                         
      calcDRolls n 
                  | n <=  0 = 0
                  | n ==  1 = 1
                  | n ==  2 = 1
                  | n >=  3 = 2

      numARolls = calcARolls aCount

      numDRolls = calcDRolls dCount

      aRolls    = sortBy (flip compare) (evalRand (replicateM numARolls die) g)

      dRolls    = sortBy (flip compare) (evalRand (replicateM numDRolls die) g)
      
      results   = zipWith (>) aRolls dRolls -- true is attack win, false is defense win
      
      countLosses result bf 
                = if result
                  then Battlefield (attackers bf) (defenders bf - 1)
                  else Battlefield (attackers bf - 1) (defenders bf)

      tally     = foldr countLosses bf results
      
   in (tally, g)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = if defenders bf == 0 || attackers bf < 2
            then return bf
            else battle bf >>= invade


successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= 
  \r -> 
    let calc (w, _) = w / 1000
        tally bf (w,l) = if defenders bf == 0
                         then (w+1, l) 
                         else (w, l+1)
        scorecard = foldr tally (0, 0) r
    in return $ calc scorecard

