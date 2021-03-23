module Dice where

import System.Random

import Types

rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

roll2d6 :: IO Int
roll2d6 = (+) <$> rollDie <*> rollDie

rollDice :: IO DieRolls
rollDice =
  DieRolls <$> rollDie
           <*> roll2d6
           <*> roll2d6
           <*> roll2d6
           <*> roll2d6
           <*> pure Offense

coinFlip :: a -> a -> IO a
coinFlip x y = pick <$> flip
  where
    pick r
      | r == 1    = x
      | otherwise = y
    flip :: IO Int
    flip = getStdRandom(randomR(1,2))
