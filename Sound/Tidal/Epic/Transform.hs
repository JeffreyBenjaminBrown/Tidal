{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.Transform where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Concurrent
import Data.Fixed
import Data.List (sortOn, partition)
import Data.Maybe
import Data.Map hiding (map, mapMaybe, partition, foldl)
import Data.Ord
import Data.Ratio
import Data.Typeable
import Data.Function
import System.Random.Mersenne.Pure64
import qualified Data.Text as T
import Text.Show.Functions ()
import qualified Control.Exception as E

import Sound.Tidal.Dirt (dirt, superDirtBackend)
import Sound.Tidal.Time
import Sound.Tidal.Transition (transition)
import Sound.Tidal.Pattern (seqToRelOnsetDeltas, slow)
import Sound.Tidal.Stream (ticksPerCycle)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util


eSilence :: Epic a
eSilence = Epic { period = Nothing, eArc = const [] }

durSilence :: Time -> Epic a
durSilence t = Epic { period = Just t, eArc = const [] }

early, late, eSlow, eFast :: Time -> Epic a -> Epic a
early t (Epic d f) = Epic d             $ \(s,e) -> g $ f (s+t, e+t) where
  g = map $ first $ mapArc $ \x -> x-t
late  t (Epic d f) = Epic d             $ \(s,e) -> g $ f (s-t, e-t) where
  g = map $ first $ mapArc $ \x -> x+t
eSlow t (Epic d f) = Epic (fmap (*t) d) $ \(s,e) -> g $ f (s/t, e/t) where
  g = map $ first $ mapArc $ \x -> x*t
eFast t (Epic d f) = Epic (fmap (/t) d) $ \(s,e) -> g $ f (s*t, e*t) where
  g = map $ first $ mapArc $ \x -> x/t

eRev :: Epic a -> Epic a
eRev    (Epic d f) = Epic d $ \(s,e) -> reverse $ g $ f (-e, -s)
  -- reversal sorts by first element, because takeOverlappingEvs needs that
  where g = map $ first $ \(s,e) -> (-e,-s)

eDur :: Time -> a -> Epic a
eDur t x = Epic Nothing $ \arc ->
  let ov = overlap arc (0,t)
  in maybe [] (\arc -> [(arc,x)]) ov

ever :: a -> Epic a
ever a = Epic Nothing $ \arc -> [(arc,a)]

eInstant :: a -> Epic a
eInstant = eDur 0

-- | Repeats the portion of an Epic from 0 to `dur`. PITFALL:
-- Information (if any) originally in the `period` field is not preserved.
eRepeat :: Time -> Epic a -> Epic a
eRepeat dur e = Epic dur' fd' where
  Epic dur' fd = _eRepeat dur e
  fd' (s,e) = let cycle = floor $ fromRational $ s/dur
                  cyclesToAdd = (-1) * min 0 (fromIntegral cycle)
                  toPositive t = t + dur * cyclesToAdd
                  fromPositive t = t - dur * cyclesToAdd
    in map (first $ mapArc fromPositive) $ fd $ mapArc toPositive (s,e)

-- | wrong when s < 0; use eRepeat instead.
_eRepeat :: Time -> Epic a -> Epic a
_eRepeat dur (Epic _ fe) = Epic (Just dur) fd where
  fd (s,e) =
    let cycle = floor $ fromRational $ s/dur
        cycleStart = dur * fromIntegral cycle
        nextCycleStart = cycleStart + dur
        toFirstCycle :: Time -> Time
        toFirstCycle t = t - fromIntegral cycle * dur
        fromFirstCycle :: Time -> Time
        fromFirstCycle t = t + fromIntegral cycle * dur
        fromFirstCycle' :: (Arc,a) -> (Arc,a)
        fromFirstCycle' = first $ mapArc fromFirstCycle
        lastCycle = e <= nextCycleStart :: Bool
        e' = if lastCycle then toFirstCycle e else nextCycleStart
        rest = if lastCycle then [] else fd (nextCycleStart,e)
    in (map fromFirstCycle' $ fe (toFirstCycle s, e')) ++ rest

window :: Arc -> Epic a -> Epic a
window win (Epic _ f) = Epic Nothing $ \arc -> maybe [] f $ overlap win arc
