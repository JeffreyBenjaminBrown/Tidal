module Sound.Tidal.Vector.Types where

import qualified Data.List as L
import qualified Data.Vector as V
import Sound.Tidal.Vector.Types.Reimports


type Ev a = (Arc,a) -- like Event, but only one arc

-- | PITFALL: These should be kept sorted (using sortDurVec).
-- For instance, dvPeriod assumes (something a bit weaker than) that.
-- (I would use dependent types if it was easy ...)
data VecEv a = VecEv { start :: Time
                     , duration :: Time
                     , payload :: Maybe a } deriving (Show, Eq)

type DurVec a = V.Vector (VecEv a)

dvDur :: V.Vector (VecEv a) -> Time
dvDur = V.foldl f 0 where f time vecEv = duration vecEv + time

sortDurVec :: V.Vector (VecEv a) -> V.Vector (VecEv a)
sortDurVec = V.fromList . L.sortOn before . V.toList
  where before v = (start v, duration v)

-- TODO : What if a long note should keep ringing into the next measure?
-- Generalizing, what if it "includes" an intro to play before it starts?
-- I think I should keep period as a separate parameter. (Here it's convenient
-- that period means something distinct from duration.)
-- Also, to avoid name conflicts, start a new branch, without Epic.
dvPeriod :: V.Vector (VecEv a) -> Time
dvPeriod v = let e = V.last v in start e + duration e
