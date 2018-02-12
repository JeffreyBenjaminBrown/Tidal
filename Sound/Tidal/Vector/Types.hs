{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Vector.Types where

import qualified Data.List as L
import qualified Data.Vector as V
import Sound.Tidal.Vector.Types.Reimports
import Control.Lens


type Ev a = (Arc,a) -- like Event, but only one arc

-- | An event with durations; what DurVecs are made of.
-- PITFALL: These should be kept sorted (using sortDurVec).
-- For instance, dvPeriod assumes (something a bit weaker than) that.
-- (I would use dependent types if it was easy ...)
data VecEv a = VecEv { veStart :: Time
                     , veDuration :: Time
                     , vePayload :: a } deriving (Show, Eq)

-- | Overlapping events, empty periods of time, and notes that
-- outlast the period of the DurVec are all fine.
data DurVec a = DurVec { _dvPeriod :: Time
                       , _dvPayload :: V.Vector (VecEv a) }
makeLenses ''DurVec

sortDurVec :: V.Vector (VecEv a) -> V.Vector (VecEv a)
sortDurVec = V.fromList . L.sortOn before . V.toList
  where before v = (veStart v, veDuration v)
        -- before here is not a function, just an Ord

silence :: DurVec ParamMap
silence = DurVec { _dvPeriod = 1, _dvPayload = mempty }
