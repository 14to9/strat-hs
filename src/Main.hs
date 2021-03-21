{-# LANGUAGE NegativeLiterals #-}

module Main where

import System.Random

-- Extra tight integer synonyms
type Yards      = Int
type CardRoll   = Int
type LookupRoll = Int
type TestRoll   = Int
type TestRange  = [TestRoll]
type DefRating  = Int
type ZoneCount  = Int
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
    CompletionChance TestRange Yardage
  | CompletionTest Position Yardage
  | GainChance TestRange Yardage Yardage
  | InterceptionChance TestRange Yardage
  | PositionTest Position Yardage Yardage
  deriving Show

--
-- Positions in CardResult conditions
--
data Position =
    DefensiveOnsideEnd
  | DefenderInZone
  | OffensiveOnsideGuard
  | OffensiveOnsideEnd
  | OffensiveOnsideTackle
  | BlockingBack
  | ManCoverageDefender
  deriving Show

--
--
-- Card types
-- Every value on stat cards (offense and defense) is a CardResult
--
data CardResult =
    Incompletion
  | Completion Yardage -- credits QB
  | Catch Yardage      -- credits WR
  | Dropped
  | MustRun
  | ReceiverRoll
  | Test CardConditional
  | IllegalCall
  deriving Show

type CardColumn = LookupRoll -> CardResult

data ReceiverCard = ReceiverCard { wrFlatPassCorrect :: CardColumn}

data PasserCard   = PasserCard   { qbFlatPassCorrect :: CardColumn}

data DefenseCard  = DefenseCard { defFlatZero :: CardColumn
                                , defFlatOne  :: CardColumn
                                }

--
-- Play elements
--
data PlayResult =
    IncompletePass
  | CompletedPass Yardage
  | Interception Yardage
  | PasserScrambled Yardage
  | NearlyIntercepted
  | NoCatch
  | PassDefensed
  | NoPlay
  deriving Show

data Zone = Flat

data Side = OffensiveRight
          | OffensiveLeft
  deriving Show

data PlayCall =
  Pass Zone Side PasserCard ReceiverCard

data DefensiveCall =
    StopPass
  | StopRun
  deriving Show

data DefensiveAlignment = DefensiveAlignment { flatLeftZone  :: [DefRating]
                                             , flatRightZone :: [DefRating]
                                             }

data Play = Play { pCall :: PlayCall
                 , dCall :: DefensiveCall
                 , dAlignment :: DefensiveAlignment
                 }

--
-- We will generate the set of die rolls for every flow path
-- and pass this master set on every play.
--
data DieRolls = DieRolls { cardRoll         :: CardRoll
                         , playRoll         :: LookupRoll
                         , receiverRoll     :: LookupRoll
                         , interceptionRoll :: TestRoll
                         , gainRoll         :: TestRoll
                         } deriving Show

--
-- Functions that apply tables to rolls
--

runPlay :: DefenseCard -> Play -> DieRolls -> PlayResult
runPlay dc p r
  | ((cardRoll r) < 4) = cardResult (startColumnOffense p $ playRoll r) p r
  | otherwise          = cardResult (startColumnDefense dc p $ playRoll r) p r

startColumnOffense :: Play -> CardColumn
startColumnOffense (Play (Pass Flat _ qb _) StopPass _) = qbFlatPassCorrect qb
startColumnOffense _ = illegalCallColumn

defendersInZone :: Zone -> Side -> DefensiveAlignment -> ZoneCount
defendersInZone Flat OffensiveLeft  = zoneCount flatRightZone
defendersInZone Flat OffensiveRight = zoneCount flatLeftZone

zoneCount :: (DefensiveAlignment -> [DefRating]) -> DefensiveAlignment -> ZoneCount
zoneCount f a = length $ f a

startColumnDefense :: DefenseCard -> Play -> CardColumn
startColumnDefense def (Play (Pass Flat side _ _) _ align)
  | (defendersInZone Flat side align) == 0 = defFlatZero def
  | (defendersInZone Flat side align) == 1 = defFlatOne def
  | otherwise                              = illegalCallColumn

receiverColumn :: Play -> CardColumn
receiverColumn (Play (Pass Flat _ _ wr) StopPass _) = wrFlatPassCorrect wr
receiverColumn _ = illegalCallColumn

-- Main lookup resolution function will recurse when redirect to other columns
cardResult :: CardResult -> Play -> DieRolls -> PlayResult
cardResult (Completion y) _ _ = CompletedPass y
cardResult Dropped _ _        = NoCatch
cardResult IllegalCall _ _    = NoPlay
cardResult Incompletion _ _   = IncompletePass
cardResult MustRun _ r        = PasserScrambled (Gain 12)
cardResult ReceiverRoll p r   = cardResult (receiverColumn p $ receiverRoll r) p r
cardResult (Test (PositionTest _ _ _)) _ _ = PassDefensed
cardResult (Test (CompletionTest _ _)) _ _ = PassDefensed
cardResult (Test (InterceptionChance range y)) _ r =
  testRange range (interceptionRoll r) (Interception y) NearlyIntercepted
cardResult (Test (GainChance range y y')) _ r =
  testRange range (gainRoll r) (CompletedPass y) (CompletedPass y')
cardResult (Test (CompletionChance range y)) _ r =
  testRange range (gainRoll r) (CompletedPass y) IncompletePass

testRange :: TestRange -> TestRoll -> PlayResult -> PlayResult -> PlayResult
testRange xs x a b
  | x `elem` xs = a
  | otherwise   = b

--
-- Simulating card data (tables)
--
illegalCallColumn :: CardColumn
illegalCallColumn _ = IllegalCall

samplePassColumn :: CardColumn
samplePassColumn 2  = Completion (Loss 1)
samplePassColumn 3  = Completion ShortGain
samplePassColumn 5  = Completion (Gain 5)
samplePassColumn 8  = ReceiverRoll
samplePassColumn 9  = ReceiverRoll
samplePassColumn 10 = Completion (Gain 4)
samplePassColumn 11 = Test (InterceptionChance [2,3] (Gain 2))
samplePassColumn _  = Incompletion

sampleRecvColumn :: CardColumn
sampleRecvColumn 3  = Test (GainChance [2,3] LongGain (Gain 3))
sampleRecvColumn 7  = Test (GainChance [2,3,4,5,6,7,8] LongGain (Gain 9))
sampleRecvColumn 8  = Completion (Gain 8)
sampleRecvColumn 9  = Completion (Gain 5)
sampleRecvColumn 10 = Completion (Gain 4)
sampleRecvColumn 11 = Completion ShortGain
sampleRecvColumn _  = Dropped

sampleFlat0 :: CardColumn
sampleFlat0 2  = Completion LongGain
sampleFlat0 3  = Completion (Gain 7)
sampleFlat0 4  = Completion (Gain 8)
sampleFlat0 5  = Completion (Gain 9)
sampleFlat0 6  = Completion (Gain 10)
sampleFlat0 7  = Completion (Gain 13)
sampleFlat0 8  = ReceiverRoll
sampleFlat0 9  = ReceiverRoll
sampleFlat0 10 = Completion (Gain 23)
sampleFlat0 11 = Completion (Gain 16)
sampleFlat0 12 = Completion (Gain 30)

sampleFlat1 :: CardColumn
sampleFlat1 2  = Test (CompletionChance [2,3,4] (Gain 4))
sampleFlat1 3  = Test (InterceptionChance [2,3,4,5,12] (Gain 3))
sampleFlat1 6  = Completion (Gain 3)
-- sampleFlat1 7  = DefenderInZone (Gain ShortGain)
sampleFlat1 8  = ReceiverRoll
sampleFlat1 9  = ReceiverRoll
sampleFlat1 10 = Completion (Gain -4)
sampleFlat1 11 = Completion (Gain 2)
sampleFlat1 12 = Completion (Gain 1)
sampleFlat1 _  = Incompletion

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

coinFlip :: a -> a -> IO a
coinFlip x y = pick <$> flip
  where
    pick r
      | r == 1    = x
      | otherwise = y
    flip :: IO Int
    flip = getStdRandom(randomR(1,2))

--
-- main
--
main :: IO ()
main = do
  randoms <- rollDice
  side    <- coinFlip OffensiveRight OffensiveLeft
  dCall   <- coinFlip StopPass StopRun

  let def   = DefenseCard sampleFlat0 sampleFlat1
      qb    = PasserCard samplePassColumn
      wr    = ReceiverCard sampleRecvColumn
      align = DefensiveAlignment { flatLeftZone  = [4]
                                 , flatRightZone = []
                                 }
      play  = Play (Pass Flat side qb wr) dCall align


  putStrLn $ show randoms
  putStrLn $ "  Pass to " <> (show side)
  putStrLn $ "  Defense calls " <> (show dCall) <> ", " <> (show $ defendersInZone Flat side align) <> " defenders in zone."
  putStrLn $ "  " <> (show $ runPlay def play randoms)
