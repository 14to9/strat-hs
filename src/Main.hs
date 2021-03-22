{-# LANGUAGE NegativeLiterals #-}

module Main where

import System.Random

-- Extra tight integer synonyms
type Yards         = Int
type CardRoll      = Int
type LookupRoll    = Int
type TestRoll      = Int
type TestRange     = [TestRoll]
type DefRating     = Int
type ZoneCount     = Int
type ZoneDefenders = [DefRating]
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
    Catch Yardage      -- WR
  | Incompletion       -- QB
  | Completion Yardage -- QB
  | MustRun
  | NoCatch            -- WR
  | PassDefensed       -- def
  | RollReceiver
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
data PassFailureReason =
    QbMiss
  | Dropped
  | GoodCoverage
  | BrokenUp Position
  | NearlyIntercepted Position
  deriving Show

data PlayResult =
    IncompletePass PassFailureReason
  | CompletedPass Yardage
  | Interception Yardage
  | PasserScrambled Yardage
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

data DefensiveAlignment = DefensiveAlignment { flatLeftZone  :: ZoneDefenders
                                             , flatRightZone :: ZoneDefenders
                                             }

data Play = Play { pCall :: PlayCall
                 , dCall :: DefensiveCall
                 , dAlignment :: DefensiveAlignment
                 }

--
-- We will generate the set of die rolls for every flow path
-- and pass this master set on every play.
--
data TeamRoll = Offense | Defense
  deriving (Show, Eq)

data DieRolls = DieRolls { positionRoll     :: CardRoll
                         , playRoll         :: LookupRoll
                         , receiverRoll     :: LookupRoll
                         , interceptionRoll :: TestRoll
                         , gainRoll         :: TestRoll
                         , recoveryRoll     :: TeamRoll
                         } deriving Show

--
-- Functions that apply tables to rolls
--

runPlay :: DefenseCard -> Play -> DieRolls -> PlayResult
runPlay dc p r
  | useOffense r = cardResult (startColumnOffense p $ playRoll r) p r
  | otherwise    = cardResult (startColumnDefense dc p $ playRoll r) p r

startColumnOffense :: Play -> CardColumn
startColumnOffense (Play (Pass Flat _ qb _) StopPass _) = qbFlatPassCorrect qb
startColumnOffense (Play (Pass Flat _ qb _) StopRun _)  = qbFlatPassCorrect qb

zoneDefenders :: Play -> ZoneDefenders
zoneDefenders (Play (Pass zone side _ _) _ align)  = defendersInAlignment zone side align

zoneCount :: Play -> ZoneCount
zoneCount = length . zoneDefenders

defendersInAlignment :: Zone -> Side -> DefensiveAlignment -> ZoneDefenders
defendersInAlignment Flat OffensiveLeft  = flatRightZone
defendersInAlignment Flat OffensiveRight = flatLeftZone

startColumnDefense :: DefenseCard -> Play -> CardColumn
startColumnDefense def play
  | (zoneCount play) == 0 = defFlatZero def
  | (zoneCount play) == 1 = defFlatOne def
  | otherwise             = illegalCallColumn

receiverColumn :: Play -> CardColumn
receiverColumn (Play (Pass Flat _ _ wr) StopPass _) = wrFlatPassCorrect wr
receiverColumn (Play (Pass Flat _ _ wr) StopRun _)  = wrFlatPassCorrect wr

-- Main lookup resolution function will recurse when redirect to other columns
cardResult :: CardResult -> Play -> DieRolls -> PlayResult
cardResult (Catch y) _ _      = CompletedPass y
cardResult (Completion y) _ _ = CompletedPass y
cardResult IllegalCall _ _    = NoPlay
cardResult Incompletion _ _   = IncompletePass QbMiss
cardResult MustRun _ r        = PasserScrambled (Gain 12)
cardResult NoCatch _ _        = IncompletePass Dropped
cardResult RollReceiver p r   = cardResult (receiverColumn p $ receiverRoll r) p r
cardResult (Test (PositionTest pos _ _)) _ _ = IncompletePass (BrokenUp pos)
cardResult (Test (InterceptionChance range y)) _ r =
  testRange range (interceptionRoll r) (Interception y) $ IncompletePass (NearlyIntercepted ManCoverageDefender)
cardResult (Test (GainChance range y y')) _ r =
  testRange range (gainRoll r) (CompletedPass y) (CompletedPass y')
cardResult (Test (CompletionChance range y)) _ r =
  testRange range (gainRoll r) (CompletedPass y) (IncompletePass GoodCoverage)
cardResult (Test (CompletionTest pos y)) play r =
  testDefender play r (IncompletePass $ BrokenUp DefenderInZone) (CompletedPass y)

testRange :: TestRange
          -> TestRoll
          -> PlayResult
          -> PlayResult
          -> PlayResult
testRange xs x hit miss
  | x `elem` xs = hit
  | otherwise   = miss

testDefender :: Play
             -> DieRolls
             -> PlayResult
             -> PlayResult
             -> PlayResult
testDefender play r hit miss
  | maximum (zoneDefenders play) >= (positionRoll r) = hit
  | otherwise                                        = miss

--
-- Simulating card data (tables)
--
illegalCallColumn :: CardColumn
illegalCallColumn _ = IllegalCall

samplePassColumn :: CardColumn
samplePassColumn 2  = Completion (Loss 1)
samplePassColumn 3  = Completion ShortGain
samplePassColumn 5  = Completion (Gain 5)
samplePassColumn 8  = RollReceiver
samplePassColumn 9  = RollReceiver
samplePassColumn 10 = Completion (Gain 4)
samplePassColumn 11 = Test (InterceptionChance [2,3] (Gain 2))
samplePassColumn _  = Incompletion

sampleRecvColumn :: CardColumn
sampleRecvColumn 2  = Test (GainChance [2,3,4,5,6,7,8] LongGain (Gain 9))
sampleRecvColumn 3  = Test (GainChance [2,3] LongGain (Gain 3))
sampleRecvColumn 8  = Catch (Gain 8)
sampleRecvColumn 9  = Catch (Gain 5)
sampleRecvColumn 10 = Catch (Gain 4)
sampleRecvColumn 11 = Catch ShortGain
sampleRecvColumn _  = NoCatch

sampleFlat0 :: CardColumn
sampleFlat0 2  = Completion LongGain
sampleFlat0 3  = Completion (Gain 7)
sampleFlat0 4  = Completion (Gain 8)
sampleFlat0 5  = Completion (Gain 9)
sampleFlat0 6  = Completion (Gain 10)
sampleFlat0 7  = Completion (Gain 13)
sampleFlat0 8  = RollReceiver
sampleFlat0 9  = RollReceiver
sampleFlat0 10 = Completion (Gain 23)
sampleFlat0 11 = Completion (Gain 16)
sampleFlat0 12 = Completion (Gain 30)

sampleFlat1 :: CardColumn
sampleFlat1 2  = Test (CompletionChance [2,3,4] (Gain 4))
sampleFlat1 3  = Test (InterceptionChance [2,3,4,5,12] (Gain 3))
sampleFlat1 6  = Completion (Gain 3)
sampleFlat1 7  = Test (CompletionTest DefenderInZone ShortGain)
sampleFlat1 8  = RollReceiver
sampleFlat1 9  = RollReceiver
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
           <*> pure Offense

coinFlip :: a -> a -> IO a
coinFlip x y = pick <$> flip
  where
    pick r
      | r == 1    = x
      | otherwise = y
    flip :: IO Int
    flip = getStdRandom(randomR(1,2))

whichTeam :: CardRoll -> TeamRoll
whichTeam r
  | (r < 4)   = Offense
  | otherwise = Defense

useOffense :: DieRolls -> Bool
useOffense r = whichTeam (positionRoll r) == Offense

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
  putStrLn $ "  Defense calls " <> (show dCall) <> ", " <> (show $ length $ zoneDefenders play) <> " defenders in zone."
  putStrLn $ "  " <> (show $ runPlay def play randoms)
