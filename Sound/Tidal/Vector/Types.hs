{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Vector.Types where

import qualified Data.List as L
import qualified Data.Vector as V
import Sound.Tidal.Vector.Types.Reimports
import Control.Lens

import Control.Monad.ST
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Mutable
import Data.Ord (Ordering(..))


type Ev a = (Arc,a) -- like Event, but only one arc

-- | An event with durations; what DurVecs are made of.
-- PITFALL: These should be kept sorted (using sortDurVec).
-- For instance, dvPeriod assumes (something a bit weaker than) that.
-- (I would use dependent types if it was easy ...)
data VecEv a = VecEv { veStart :: Time
                     , veDuration :: Time
                     , vePayload :: a } deriving (Show, Eq)

-- | Overlapping events, empty periods of time, and notes that outlast
-- the period of the DurVec are all fine. And maybe ones preceding it, too.
data DurVec a = DurVec { _dvPeriod :: Time
                       , _dvPayload :: V.Vector (VecEv a) }
              deriving Eq
makeLenses ''DurVec

sortDurVec' :: DurVec a -> DurVec a
sortDurVec' = dvPayload %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare a a' where
                         a  = (veStart ve , veDuration ve )
                         a' = (veStart ve', veDuration ve')
                   sortBy compare' v'
                   V.freeze v'

silence :: DurVec ParamMap
silence = DurVec { _dvPeriod = 1, _dvPayload = mempty }
