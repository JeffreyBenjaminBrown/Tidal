{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.Transform where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Concurrent
import Data.Fixed
import Data.List (sortOn, partition)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
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
import Sound.Tidal.Pattern (seqToRelOnsetDeltas)
import Sound.Tidal.Stream (ticksPerCycle)

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util


silence :: Epic a
silence = Epic { _period = Nothing, _arc = const [] }

durSilence :: Time -> Epic a
durSilence t = Epic { _period = Just t, _arc = const [] }

early, late, slow, fast, dense, sparse, rotate, rep
  :: Time -> Epic a -> Epic a
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
rotate t = fast t . sparse t
rep n = slow n . dense n

-- | Originally, `rev` looked like this:
-- rev' :: Epic a -> Epic a
-- rev' (Epic d f) = Epic d $ \arc ->
--   let reflect (s,e) = (-e,-s)
--   in reverse -- sort by first element, which takeOverlappingEvs requires
--        $ map (first reflect) $ f $ reflect arc
-- But that gives the events in [-e,-s), whereas we need (-e,-s].
-- Hence the complications below.
rev :: forall a. Epic a -> Epic a
rev (Epic d f) = Epic d $ \(s,e) -> 
  let epsilon = 1%256 :: Time
      evs = f (-e,-s+epsilon) :: [(Arc,a)]
      keeper :: Time -> Bool
      keeper t = (t > -e) && (t <= -s) 
      evsKept = filter (keeper . fst . fst) evs
  in reverse -- sort by first element, which takeOverlappingEvs requires
       $ map (first $ \(s,e)->(-e,-s)) evsKept

-- | use the value from the old param in the new param
chParam :: Param -> Param -> ParamMap -> ParamMap
chParam old new m = mergeNumParamsWith (*) (*) mOtherParams mQf
  where (mSpeed,mOtherParams) = M.partitionWithKey g m
        mQf = if null mSpeed then M.empty
              else M.singleton new $ (M.!) mSpeed old
        g k _ = if k == old then True else False
        -- PITFALL: It seems like "g deg_p _ = True; g _ _ = False"
        -- ought to work, but somehow no.

-- | use the value from the old param in the new param
chVF :: Param -> (Double -> Double) -> ParamMap -> ParamMap
chVF key f = M.mapWithKey g where
  g :: Param -> Value -> Value
  g k val@(VF v) = if k == key then VF $ f v else val
  g k val        = if k == key then error $ show k ++ "holds no VF" else val

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

-- | map Time t to Time t + k sin t
-- Since Time = Rational, must approximate the result
-- ASSUMES k small enough that the mapping is invertible
warp :: forall a. Float -> Time -> Time -> Epic a -> Epic a
warp tolerance phase90 period (Epic d e) = Epic d f where
  close = approxRational tolerance
  thisWarp = warpTime' tolerance phase90 period
  warpBoth :: Arc -> Arc
  warpBoth (s,t) = (thisWarp s, thisWarp t)
  f :: Arc -> [(Arc,a)]
  f (s,t) = takeOverlappingEvs (s,t) $ map (first warpBoth) $ e (s',t')
    where (s',t') = (s - period, t + period)

-- | (warp p90 t p) maps Time t such that
-- (1) It will be the same at every multiple of p/2 (phase = 0, 180 degrees)
-- (2) The value it used to take at phase=90 degrees, it now takes at
-- (3) phase=d degrees, where d = 2 pi * p90
warpTime' :: Float -> Time -> Time -> Time -> Time
warpTime' tolerance phase90 period =
  let strength = period * (phase90 - 1%4)
  in warpTime tolerance (fromRational strength) period

-- | (warp s t p) maps Time t to Time t + s sin (2 pi t / p)
warpTime :: Float -> Float -> Time -> Time -> Time
warpTime tolerance strength period t =
  let close = flip approxRational tolerance
  in t + (close $ strength * sin (
             fromRational t * 2 * pi / fromRational period))

-- | = remap* is to make an abstract map concrete (e.g. into a ParamMap)
-- For usage examples, see Test.hs
-- To remap doubles (without keys), use M.!
-- To remap a map's keys and leave its values unchanged, use Util.composeMaps

-- | The most abstract possible variety of remap
remap :: Ord k2 => (k1->k2) -> (v1->v2) -> M.Map k1 v1 -> M.Map k2 v2
remap sf df = M.mapKeys sf . M.map df

-- | Mq: The first arg is a map and the second is any arbitrary function
remapMq :: (Ord k1,Ord k2) =>
          M.Map k1 k2 -> (v1->v2) -> M.Map k1 v1    -> M.Map k2 v2
remapMq sm df = composeMaps sm . M.map df

-- | Md: The first arg is a map and the values, Doubles, get wrapped in VF
remapMd :: (Ord k, Ord k') =>
          M.Map k k'              -> M.Map k Double -> M.Map k' Value
remapMd = flip remapMq VF

-- | Pd: The first arg is a Param and the values, Doubles, get wrapped in VF
remapPd :: Param                   -> Double         -> ParamMap
remapPd par = M.singleton par . VF

remapPs :: Param                   -> String         -> ParamMap
remapPs par = M.singleton par . VS

-- | Usually, define k = defaultMap a as, then map k over an Epic Double
defaultMap :: [a] -> Double -> a
defaultMap (dfault:as) d =
  maybe dfault id  $  M.lookup d  $  M.fromList  $  zip [0..] as


-- | == Space

-- | "Space" a loop of length smallDur to cover a loop of length bigDur.
-- First play the loop, then silence; repeat at next multiple of bigDur.
-- Purpose: if an epic has a glue-duration distinct from its true duration,
-- e.g. thanks to "sparse", concat will still work as expected.
-- Example: Imagine spaceing an epic E of length S=2 across a length B=5.
-- The interval (0,2) is unchanged from E.
-- The interval (2,5) is silent.
-- The interval (5,7) carries what (2,4) carries under E.
-- The interval (7,10) is silent ...
--
-- ASSUMES b > ed. You can space a small loop into a bigger interval;
-- the reverse doesn't make obvious sense.

space :: Time -> Epic a -> Epic a
space bigDur (Epic Nothing _) = error "Space is only defined for loops."
space bigDur (Epic (Just smallDur) ef) = Epic (Just bigDur) $ \(s,e) ->
  let covered = addGapsForSpace bigDur smallDur (s,e)
        -- the non-gaps, the intervals that will carry payloads
      contracted = map (contractForSpace bigDur smallDur) covered
        -- the inner intervals, on which ef is computed
  in map (first $ expandForSpace bigDur smallDur)
     $ concatMap ef contracted

space2 :: Time -> Time -> Epic a -> Epic a
space2 bigDur smallDur (Epic _ ef) = Epic (Just bigDur) $ \(s,e) ->
  let covered = addGapsForSpace bigDur smallDur (s,e)
        -- the non-gaps, the intervals that will carry payloads
      contracted = map (contractForSpace bigDur smallDur) covered
        -- the inner intervals, on which ef is computed
  in map (first $ expandForSpace bigDur smallDur)
     $ concatMap ef contracted

addGapsForSpace :: Time -> Time -> Arc -> [Arc]
addGapsForSpace big small (s,e) =
  if s >= e -- todo ? does this violate the idiom established by `overlap`?
  then []
  else let z = roundDownTo big s -- the first phase 0 before (or equal to) s
           z' = z + big -- the next phase 0
           endOne = z + small -- the end of the first covered interval
           ov = overlap (z,endOne) (s,e)
       in maybe [] (:[]) ov ++ addGapsForSpace big small (z',e)

contractForSpace :: Time -> Time -> Arc -> Arc
contractForSpace big small (s,e) =
  let smallPhase0 = small * fromIntegral (div' s big)
      diff = s - roundDownTo big s
      length = e-s
      smallStart = smallPhase0 + diff
  in (smallStart, smallStart + length)

expandForSpace :: Time -> Time -> Arc -> Arc
expandForSpace big small (s,e) = let n = fromIntegral $ div' s small
                                     r = mod' s small
                                     z = n * big -- phase zero
                                 in (z + r, z + r + e-s)
