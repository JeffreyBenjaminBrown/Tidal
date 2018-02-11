module Sound.Tidal.Vector.Types where

import qualified Data.Vector as V
import Sound.Tidal.Epic.Types.Reimports

data VecEv a = VecEv { start :: Time
                     , duration :: Time
                     , payload :: Maybe a }

data DurVec a = DurVec { dvDur :: Time
                       , events :: V.Vector (VecEv a) }

vecDur :: V.Vector (VecEv a) -> Time
vecDur = V.foldl f 0 where f time vecEv = duration vecEv + time
