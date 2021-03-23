module Engine where

import Types

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

whichTeam :: CardRoll -> TeamRoll
whichTeam r
  | (r < 4)   = Offense
  | otherwise = Defense

useOffense :: DieRolls -> Bool
useOffense r = whichTeam (positionRoll r) == Offense
