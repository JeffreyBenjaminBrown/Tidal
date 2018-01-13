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

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util


silence :: Epic a
silence = Epic { period = Nothing, arc = const [] }

durSilence :: Time -> Epic a
durSilence t = Epic { period = Just t, arc = const [] }

early, late, slow, fast, dense, sparse :: Time -> Epic a -> Epic a
early t (Epic d f) = Epic d             $ \(s,e) -> g $ f (s+t, e+t) where
  g = map $ first $ mapArc $ \x -> x-t
late  t (Epic d f) = Epic d             $ \(s,e) -> g $ f (s-t, e-t) where
  g = map $ first $ mapArc $ \x -> x+t
slow t (Epic d f) = Epic (fmap (*t) d) $ \(s,e) -> g $ f (s/t, e/t) where
  g = map $ first $ mapArc $ \x -> x*t
fast t (Epic d f) = Epic (fmap (/t) d) $ \(s,e) -> g $ f (s*t, e*t) where
  g = map $ first $ mapArc $ \x -> x/t
dense t (Epic d f) = Epic d             $ \(s,e) -> g $ f (s*t, e*t) where
  g = map $ first $ mapArc $ \x -> x/t
sparse t (Epic d f) = Epic d            $ \(s,e) -> g $ f (s/t, e/t) where
  g = map $ first $ mapArc $ \x -> x*t

rev :: Epic a -> Epic a
rev    (Epic d f) = Epic d $ \(s,e) -> reverse $ g $ f (-e, -s)
  -- reversal sorts by first element, because takeOverlappingEvs needs that
  where g = map $ first $ \(s,e) -> (-e,-s)

for :: Time -> a -> Epic a
for t x = Epic Nothing $ \arc ->
  let ov = overlap arc (0,t)
  in maybe [] (\arc -> [(arc,x)]) ov

ever :: a -> Epic a
ever a = Epic Nothing $ \arc -> [(arc,a)]

instant :: a -> Epic a
instant = for 0

-- | = loop* repeats the portion of an Epic from 0 to `dur`. PITFALL:
-- Information (if any) originally in the `period` field is not preserved.
loopa, loop0 :: Time -> a      -> Epic a
loopa dur = loope dur . for dur
loop0 dur = loope dur . for 0

loope :: Time -> Epic a -> Epic a
loope dur e = Epic dur' fd' where
  Epic dur' fd = _loope dur e
  fd' (s,e) = let cycle = floor $ fromRational $ s/dur
                  cyclesToAdd = (-1) * min 0 (fromIntegral cycle)
                  toPositive t = t + dur * cyclesToAdd
                  fromPositive t = t - dur * cyclesToAdd
    in map (first $ mapArc fromPositive) $ fd $ mapArc toPositive (s,e)

-- | wrong when s < 0; use loope instead.
_loope :: Time -> Epic a -> Epic a
_loope dur (Epic _ fe) = Epic (Just dur) fd where
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
