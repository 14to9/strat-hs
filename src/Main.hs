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
passResult ReceiverRoll r   = CompletedPass (Gain 22)
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

main :: IO ()
main = do
  let randoms = Randoms { playRoll         = 11
                        , interceptionRoll = 2
                        , receiverRoll     = 4
                        }

  putStrLn $ show $ attemptPass randoms flatPassRight
