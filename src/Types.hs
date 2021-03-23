module Types where

-- Extra tight integer synonyms
type Yards         = Int
type CardRoll      = Int
type LookupRoll    = Int
type TestRoll      = Int
type DefRating     = Int
type ZoneCount     = Int
type ZoneDefenders = [DefRating]
type TestRange     = [TestRoll]
type FumbleRange   = [TestRoll]
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

data OffenseCard  = OffenseCard { offFumbleRange :: FumbleRange }
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
