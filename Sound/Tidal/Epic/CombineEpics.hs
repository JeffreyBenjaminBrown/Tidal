{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.CombineEpics where

import Control.Arrow (first)
import Data.Fixed (div', mod')
import Data.List (sortOn)

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util
import Sound.Tidal.Epic.Transform


-- | The duration of `stack a b` is the LCM of the durations of `a` and `b`.
stack :: Epic a -> Epic a -> Epic a
stack (Epic df f) (Epic dg g) = Epic (lcmRatios <$> df <*> dg) $
  \arc -> sortOn (fst . fst) $ f arc ++ g arc
  -- sort by first element because takeOverlappingEvs needs that

stacka :: Time -> [a] -> Epic a
stacka t = foldr1 stack . fmap (loopa t)

-- | Only repeating Epics are concatenable.
-- TODO: handle finite-duration non-repeating Epics too.
append :: Epic a -> Epic a -> Epic a
append (Epic Nothing _) _ = error "append requires repeating patterns"
append e1@(Epic (Just t1) f1) e2@(Epic (Just t2) f2) =
  let e1' =           spread (t1+t2) e1
      e2' = late t1 $ spread (t1+t2) e2
  in stack e1' e2'

-- | "Spread" a loop of length smallDur to cover a loop of length bigDur.
-- First play the loop, then silence; repeat at next multiple of bigDur.
-- Purpose: if an epic has a glue-duration distinct from its true duration,
-- e.g. thanks to "sparse", concat will still work as expected.
-- Example: Imagine spreading an epic E of length S=2 across a length B=5.
-- The interval (0,2) is unchanged from E.
-- The interval (2,5) is silent.
-- The interval (5,7) carries what (2,4) carries under E.
-- The interval (7,10) is silent ...
--
-- ASSUMES b > ed. You can spread a small loop into a bigger interval;
-- the reverse doesn't make obvious sense.

spread :: Time -> Epic a -> Epic a
spread bigDur (Epic Nothing _) = error "Spread is only defined for loops."
spread bigDur (Epic (Just smallDur) ef) = Epic (Just bigDur) $ \(s,e) ->
  let covered = addGapsForSpread bigDur smallDur (s,e)
        -- the non-gaps, the intervals that will carry payloads
      contracted = map (contractForSpread bigDur smallDur) covered
        -- the inner intervals, on which ef is computed
  in map (first $ expandForSpread bigDur smallDur)
     $ concatMap ef contracted

spread2 :: Time -> Time -> Epic a -> Epic a
spread2 bigDur smallDur (Epic _ ef) = Epic (Just bigDur) $ \(s,e) ->
  let covered = addGapsForSpread bigDur smallDur (s,e)
        -- the non-gaps, the intervals that will carry payloads
      contracted = map (contractForSpread bigDur smallDur) covered
        -- the inner intervals, on which ef is computed
  in map (first $ expandForSpread bigDur smallDur)
     $ concatMap ef contracted

addGapsForSpread :: Time -> Time -> Arc -> [Arc]
addGapsForSpread big small (s,e) =
  if s >= e -- todo ? does this violate the idiom established by `overlap`?
  then []
  else let z = roundDownTo big s -- the first phase 0 before (or equal to) s
           z' = z + big -- the next phase 0
           endOne = z + small -- the end of the first covered interval
           ov = overlap (z,endOne) (s,e)
       in maybe [] (:[]) ov ++ addGapsForSpread big small (z',e)

contractForSpread :: Time -> Time -> Arc -> Arc
contractForSpread big small (s,e) =
  let smallPhase0 = small * fromIntegral (div' s big)
      diff = s - roundDownTo big s
      length = e-s
      smallStart = smallPhase0 + diff
  in (smallStart, smallStart + length)

expandForSpread :: Time -> Time -> Arc -> Arc
expandForSpread big small (s,e) = let n = fromIntegral $ div' s small
                                      r = mod' s small
                                      z = n * big -- phase zero
                                  in (z + r, z + r + e-s)

mergeEpics :: (Int->Int->Int) -> (Double->Double-> Double) ->
              ParamEpic -> ParamEpic -> ParamEpic
mergeEpics intOp floatOp (Epic ap af) (Epic bp bf) = Epic period func where
  period = lcmRatios <$> ap <*> bp
  func arc = mergeEvents intOp floatOp aEvs bEvs where
    s = sortOn $ fst . fst :: [Ev a] -> [Ev a]
      -- AMBITION ? if ParamEpics were guaranteed to produce sorted lists,
      -- aEvs and bEvs would not have to use `s`
    aEvs = s $ af arc :: [Ev ParamMap]
    bEvs = s $ bf arc :: [Ev ParamMap]

mergeEvents :: (Int->Int->Int) -> (Double->Double-> Double) ->
               [Ev ParamMap] -> [Ev ParamMap] -> [Ev ParamMap]
mergeEvents intOp floatOp aEvs bEvs =  k aEvs' bEvs' where
  bs = boundaries $ map fst $ aEvs ++ bEvs
  aEvs' = partitionAndGroupEventsAtBoundaries bs aEvs
  bEvs' = partitionAndGroupEventsAtBoundaries bs bEvs
  k,k' :: [Ev ParamMap] -> [Ev ParamMap] -> [Ev ParamMap]
  k [] bEvs = []
  k aEvs [] = []
  k  aEvs@((arcA,_):aEvsRest)  bEvs@((arcB,_):bEvsRest)
    | arcA <  arcB = k aEvsRest bEvs
    | arcB <  arcA = k aEvs bEvsRest
    | arcA == arcB = k' aEvs bEvs
  k' ((arc,a):aEvs) bEvs = merge ++ k aEvs bEvs where
    bEvsMatch = takeWhile ((== arc) . fst) bEvs
    merge = (arc,) . mergeNumParamsWith intOp floatOp a . snd <$> bEvsMatch

-- | applyMetaEpic
meta :: Epic (Epic a -> Epic b) -> Epic a -> Epic b
meta    (Epic md mf)               obj    = Epic d' f' where
  d' = lcmRatios <$> md <*> _period obj
  -- f' :: Arc -> [Ev b]
  f' a = concatMap h transformEvs
    where -- transformEvs :: [(Arc, Epic a -> Epic b)]
          transformEvs = mf a
          -- h :: (Arc, Epic a -> Epic b) -> [Ev b]
          h (theArc,tr) = _arc (tr obj) theArc
