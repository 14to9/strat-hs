module Main where

import Dice (coinFlip, rollDice)
import Engine (snap, runPlay, zoneCount, guessResult)
import SampleTeam
import Types

--
-- main
--
main :: IO ()
main = do
  -- This sets up a random play call and should probably instead live in a test
  side  <- coinFlip OffensiveRight OffensiveLeft
  focus <- coinFlip StopPass StopRun

  let oCall = Pass Flat side samplePasserCard sampleReceiverCard
      dCall = DefensiveCall focus sampleDefenseCard sampleAlignment
      guess = guessResult oCall dCall
      play  = Play oCall dCall guess

  -- Roll dice and evaluate
  randoms <- rollDice
  let atSnap = snap play
      result = runPlay atSnap randoms

  putStrLn $ show randoms
  putStrLn $ "  Pass to " <> (show side)
  putStrLn $ "  " <> (show $ dcFocus dCall) <> ", " <> (show $ zoneCount play) <> " defenders in zone."
  putStrLn $ "  " <> (show $ result)
