module Sound.Tidal.Vector.Types where

import qualified Data.Vector as V
import Sound.Tidal.Epic.Types.Reimports

data VecEv a = VecEv { start :: Time
                     , duration :: Time
                     , payload :: Maybe a }

-- | a Vector's duration could take a long time to calculate, and
-- (I experimented) it gets recalculated every time it's asked for;
-- better to store it in dvDur.
data DurVec a = DurVec { dvDur :: Time
                       , events :: V.Vector (VecEv a) }

vecDur :: V.Vector (VecEv a) -> Time
vecDur = V.foldl f 0 where f time vecEv = duration vecEv + time

checkDurVec :: V.Vector (VecEv a) -> Bool
checkDurVec v = let durs = V.map duration v
                    starts = V.map start v
                    starts' = V.scanl (+) 0 durs -- has an extra element
                    starts'' = V.slice 0 (V.length starts' - 1) starts'
                in starts == starts''
