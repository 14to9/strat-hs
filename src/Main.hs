{-# LANGUAGE NegativeLiterals #-}

module Main where

data Yardage =
    Gain Int
  | ShortGain
  | LongGain
  deriving (Show)

data PassOutcome =
    Incompletion
  | Completion Yardage
  | MustRun
  | ReceiverRoll
  | InterceptionChance [Int] Yardage
  deriving Show

data ReceiverOutcome =
    Reception Yardage
  | NoCatch
  deriving Show

data PlayResult =
    IncompletePass
  | CompletedPass Yardage
  | Interception Yardage
  | PasserScrambled Yardage
  deriving Show

data Randoms = Randoms { playRoll         :: Int
                       , interceptionRoll :: Int
                       , receiverRoll     :: Int
                       } deriving Show

attemptPass :: Randoms -> (Int -> PassOutcome) -> PlayResult
attemptPass rolls table =
  passResult outcome rolls
  where
    outcome = table $ playRoll rolls

passResult :: PassOutcome -> Randoms -> PlayResult
passResult (Completion y) _ = CompletedPass y
passResult Incompletion _   = IncompletePass
passResult MustRun r        = PasserScrambled (Gain 12)
passResult ReceiverRoll r   = receiverResult $ receiverFlatRight (receiverRoll r)
passResult (InterceptionChance range y) r
  | (interceptionRoll r) `elem` range = Interception y
  | otherwise = IncompletePass

flatPassRight :: Int -> PassOutcome
flatPassRight 2  = Completion (Gain -1)
flatPassRight 3  = Completion ShortGain
flatPassRight 5  = Completion (Gain 5)
flatPassRight 8  = ReceiverRoll
flatPassRight 9  = ReceiverRoll
flatPassRight 10 = Completion (Gain 4)
flatPassRight 11 = InterceptionChance [2,3] (Gain 2)
flatPassRight _  = Incompletion

receiverResult :: ReceiverOutcome -> PlayResult
receiverResult (Reception y) = CompletedPass y
receiverResult NoCatch       = IncompletePass

receiverFlatRight :: Int -> ReceiverOutcome
receiverFlatRight 3  = Reception (Gain 9)
receiverFlatRight 7  = Reception (Gain 6)
receiverFlatRight 8  = Reception (Gain 8)
receiverFlatRight 9  = Reception (Gain 5)
receiverFlatRight 10 = Reception (Gain 4)
receiverFlatRight 11 = Reception ShortGain
receiverFlatRight _  = NoCatch

main :: IO ()
main = do
  let randoms = Randoms { playRoll         = 8
                        , interceptionRoll = 2
                        , receiverRoll     = 9
                        }

  putStrLn $ show $ attemptPass randoms flatPassRight
