{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Vector.Types where

import qualified Data.List as L
import qualified Data.Vector as V
import Control.Lens
import Control.Monad.ST
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Mutable

import Sound.Tidal.Vector.Types.Reimports


type Ev a = (Arc,a) -- like Event, but only one arc

-- | An event with durations; what DurVecs are made of.
-- PITFALL: These should be kept sorted (using sortDurVec).
-- For instance, dvPeriod assumes (something a bit weaker than) that.
-- (I would use dependent types if it was easy ...)
data VecEv a = VecEv { veStart :: Time
                     , veDuration :: Time
                     , vePayload :: a } deriving (Eq,Show)

vecEv :: Time -> Time -> a -> VecEv a
vecEv s d p = VecEv {veStart = s, veDuration = d, vePayload = p}

-- | Overlapping events, empty periods of time, and notes that outlast
-- the period of the DurVec are all fine. And maybe ones preceding it, too.
data DurVec a = DurVec { _dvPeriod :: Time
                       , _dvPayload :: V.Vector (VecEv a) }
              deriving (Eq,Show)
makeLenses ''DurVec

durVec :: Time -> [VecEv a] -> DurVec a
durVec t l = DurVec {_dvPeriod = t, _dvPayload = V.fromList l}
