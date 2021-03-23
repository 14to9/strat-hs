module Main where

import Dice (coinFlip, rollDice)
import Engine (runPlay, zoneCount)
import SampleTeam
import Types

--
-- main
--
main :: IO ()
main = do
  -- This sets up a random play call and should probably instead live in a test
  side    <- coinFlip OffensiveRight OffensiveLeft
  dCall   <- coinFlip StopPass StopRun

  let call  = Pass Flat side samplePasserCard sampleReceiverCard
      play  = Play call dCall sampleAlignment

  -- Roll dice and evaluate
  randoms <- rollDice
  let result = runPlay sampleTeamDefenseCard play randoms

  putStrLn $ show randoms
  putStrLn $ "  Pass to " <> (show side)
  putStrLn $ "  Defense calls " <> (show dCall) <> ", " <> (show $ zoneCount play) <> " defenders in zone."
  putStrLn $ "  " <> (show $ result)
