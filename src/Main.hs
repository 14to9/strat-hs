{-# LANGUAGE NegativeLiterals #-}

module Main where

import System.Random

-- Extra tight integer synonyms
type Yards      = Int
type CardRoll   = Int
type LookupRoll = Int
type TestRoll   = Int
type TestRange  = [Int]

--
-- Yardage results are sometimes specified, sometimes randomized
--
data Yardage =
    Gain Yards
  | Loss Yards
  | ShortGain
  | LongGain
  | NoGain
  deriving (Show)

--
-- CardConditional wraps the many if-A-then-B results on cards
--
data CardConditional =
    InterceptionChance TestRange Yardage
  | GainChance TestRange Yardage Yardage
  | PositionTest Position Yardage Yardage
  | CompletionTest Position Yardage
  deriving Show

--
-- Positions in CardResult conditions
--
data Position =
    DefensiveOnsideEnd
  | LinebackerInZone
  | OffensiveOnsideGuard
  | OffensiveOnsideEnd
  | OffensiveOnsideTackle
  | BlockingBack
  | Defender
  deriving Show
--
-- Every value on stat cards (offense and defense) is a CardResult
--
data CardResult =
    Incompletion
  | Completion Yardage
  | MustRun
  | ReceiverRoll
  | Test CardConditional
  deriving Show

type CardColumn = Int -> CardResult

--
-- Play elements
--
data PlayResult =
    IncompletePass
  | CompletedPass Yardage
  | Interception Yardage
  | PasserScrambled Yardage
  deriving Show

data PassType = FlatPass

data PlayCall =
  Pass PassType PasserCard ReceiverCard

--
-- We will generate the set of die rolls for every flow path
-- and pass this master set on every play.
--
data DieRolls = DieRolls { cardRoll         :: CardRoll
                         , playRoll         :: Int
                         , interceptionRoll :: Int
                         , receiverRoll     :: Int
                         , gainRoll         :: Int
                         } deriving Show

--
-- Functions that apply tables to rolls
--
runPlay :: PlayCall -> DieRolls -> PlayResult
runPlay (Pass t qb wr) rolls =
  passResult (qbPassColumn t qb $ playRoll rolls) rolls

qbPassColumn :: PassType -> PasserCard -> CardColumn
qbPassColumn FlatPass qb = qbFlatPassCorrect qb

receiverResult :: CardResult -> PlayResult
receiverResult (Completion y) = CompletedPass y
receiverResult _              = IncompletePass

passResult :: CardResult -> DieRolls -> PlayResult
passResult (Completion y) _ = CompletedPass y
passResult Incompletion _   = IncompletePass
passResult MustRun r        = PasserScrambled (Gain 12)
passResult ReceiverRoll r   = receiverResult $ sampleRecvTable (receiverRoll r)
passResult (Test (InterceptionChance range y)) r
  | (interceptionRoll r) `elem` range = Interception y
  | otherwise                         = IncompletePass
passResult _ _ = IncompletePass

--
-- Card types
--
data ReceiverCard = ReceiverCard { wrFlatPassCorrect :: CardColumn}

data PasserCard   = PasserCard   { qbFlatPassCorrect :: CardColumn}

--
-- Simulating card data (tables)
--
samplePassTable :: CardColumn
samplePassTable 2  = Completion (Loss 1)
samplePassTable 3  = Completion ShortGain
samplePassTable 5  = Completion (Gain 5)
samplePassTable 8  = ReceiverRoll
samplePassTable 9  = ReceiverRoll
samplePassTable 10 = Completion (Gain 4)
samplePassTable 11 = Test (InterceptionChance [2,3] (Gain 2))
samplePassTable _  = Incompletion

sampleRecvTable :: CardColumn
sampleRecvTable 3  = Completion (Gain 9)
sampleRecvTable 7  = Completion (Gain 6)
sampleRecvTable 8  = Completion (Gain 8)
sampleRecvTable 9  = Completion (Gain 5)
sampleRecvTable 10 = Completion (Gain 4)
sampleRecvTable 11 = Completion ShortGain
sampleRecvTable _  = Incompletion

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

--
-- main
--
main :: IO ()
main = do
  let qb = PasserCard samplePassTable
      wr = ReceiverCard sampleRecvTable
      play = Pass FlatPass qb wr

  randoms <- rollDice
  putStrLn $ show randoms
  putStrLn $ show $ runPlay play randoms
