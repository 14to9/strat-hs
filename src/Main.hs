{-# LANGUAGE NegativeLiterals #-}

module Main where

import System.Random

--
-- Yardage results are sometimes specified, sometimes randomized
--
data Yardage =
    Gain Int
  | Loss Int
  | ShortGain
  | LongGain
  deriving (Show)

--
-- Every value on stat cards (offense and defense) is a CardResult
--
data CardResult =
    Incompletion
  | Completion Yardage
  | MustRun
  | ReceiverRoll
  | InterceptionChance [Int] Yardage
  deriving Show

--
-- Play result is
--
data PlayResult =
    IncompletePass
  | CompletedPass Yardage
  | Interception Yardage
  | PasserScrambled Yardage
  deriving Show

--
-- We will generate the set of die rolls for every flow path
-- and pass this master set on every play.
--
data DieRolls = DieRolls { playRoll         :: Int
                         , interceptionRoll :: Int
                         , receiverRoll     :: Int
                         } deriving Show

--
-- Functions that apply tables to rolls
--
attemptPass :: DieRolls -> (Int -> CardResult) -> PlayResult
attemptPass rolls table =
  passResult outcome rolls
  where
    outcome = table $ playRoll rolls

receiverResult :: CardResult -> PlayResult
receiverResult (Completion y) = CompletedPass y
receiverResult _              = IncompletePass

passResult :: CardResult -> DieRolls -> PlayResult
passResult (Completion y) _ = CompletedPass y
passResult Incompletion _   = IncompletePass
passResult MustRun r        = PasserScrambled (Gain 12)
passResult ReceiverRoll r   = receiverResult $ receiverFlatRight (receiverRoll r)
passResult (InterceptionChance range y) r
  | (interceptionRoll r) `elem` range = Interception y
  | otherwise                         = IncompletePass

--
-- Simulating card data (tables)
--
flatPassRight :: Int -> CardResult
flatPassRight 2  = Completion (Loss 1)
flatPassRight 3  = Completion ShortGain
flatPassRight 5  = Completion (Gain 5)
flatPassRight 8  = ReceiverRoll
flatPassRight 9  = ReceiverRoll
flatPassRight 10 = Completion (Gain 4)
flatPassRight 11 = InterceptionChance [2,3] (Gain 2)
flatPassRight _  = Incompletion

receiverFlatRight :: Int -> CardResult
receiverFlatRight 3  = Completion (Gain 9)
receiverFlatRight 7  = Completion (Gain 6)
receiverFlatRight 8  = Completion (Gain 8)
receiverFlatRight 9  = Completion (Gain 5)
receiverFlatRight 10 = Completion (Gain 4)
receiverFlatRight 11 = Completion ShortGain
receiverFlatRight _  = Incompletion

rollDie :: IO Int
rollDie = getStdRandom (randomR (1,6))

roll2d6 :: IO Int
roll2d6 = do
  a <- rollDie
  b <- rollDie
  return (a + b)

rollDice :: IO DieRolls
rollDice =
  DieRolls <$> roll2d6
           <*> roll2d6
           <*> roll2d6

--
-- main
--
main :: IO ()
main = do
  randoms <- rollDice
  putStrLn $ show randoms
  putStrLn $ show $ attemptPass randoms flatPassRight
