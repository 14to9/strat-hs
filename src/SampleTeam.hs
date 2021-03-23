module SampleTeam where

import Types



sampleTeamOffenseCard :: OffenseCard
sampleTeamOffenseCard = OffenseCard { offFumbleRange = [1,2] }

sampleTeamDefenseCard :: DefenseCard
sampleTeamDefenseCard = DefenseCard sampleFlat0 sampleFlat1

samplePasserCard :: PasserCard
samplePasserCard = PasserCard samplePassColumn

sampleReceiverCard :: ReceiverCard
sampleReceiverCard = ReceiverCard sampleRecvColumn

sampleAlignment :: DefensiveAlignment
sampleAlignment = DefensiveAlignment { flatLeftZone  = [4]
                                     , flatRightZone = []
                                     }

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
sampleFlat1 10 = Completion (Loss 4)
sampleFlat1 11 = Completion (Gain 2)
sampleFlat1 12 = Completion (Gain 1)
sampleFlat1 _  = Incompletion
