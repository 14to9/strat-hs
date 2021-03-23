module Engine where

import Types

--
-- Functions that apply tables to rolls
--

runPlay :: Play -> DieRolls -> PlayResult
runPlay p r = cardResult (startColumn p $ playRoll r) p r
  where
    startColumn
      | useOffense r = startColumnOffense
      | otherwise    = startColumnDefense
--
-- snap dispatches whatever changes that are applied by rule to the
-- formation (OffRating changes) alignments (safety movements)
-- and other factors based on various call elements
-- (including changing the guess result 💀)
--
snap :: Play -> Play
-- Just pass everything through as-is right now
snap p = p

guessResult :: OffensiveCall -> DefensiveCall -> GuessResult
guessResult (Pass _ _ _ _) (DefensiveCall StopPass _ _) = GuessedRight
guessResult _ _                                         = GuessedWrong

playQb :: Play -> PasserCard
playQb =  ocQb . oCall

playReceiver :: Play -> ReceiverCard
playReceiver = ocWr . oCall

playZone :: Play -> Zone
playZone = ocZone . oCall

playSide :: Play -> Side
playSide = ocSide . oCall

playAlignment :: Play -> DefensiveAlignment
playAlignment = dcAlignment . dCall

startColumnOffense :: Play -> CardColumn
startColumnOffense = qbFlatPassCorrect . playQb

zoneDefenders :: Play -> ZoneDefenders
zoneDefenders p = defendersInAlignment (playZone p) (playSide p) (playAlignment p)

zoneCount :: Play -> ZoneCount
zoneCount = length . zoneDefenders

defendersInAlignment :: Zone -> Side -> DefensiveAlignment -> ZoneDefenders
defendersInAlignment Flat OffensiveLeft  = flatRightZone
defendersInAlignment Flat OffensiveRight = flatLeftZone

startColumnDefense :: Play -> CardColumn
startColumnDefense play
  | (zoneCount play) == 0 = defFlatZero . dcCard .dCall $ play
  | (zoneCount play) == 1 = defFlatOne . dcCard . dCall $ play
  | otherwise             = illegalCallColumn

receiverColumn :: Play -> CardColumn
receiverColumn = wrFlatPassCorrect . playReceiver

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
