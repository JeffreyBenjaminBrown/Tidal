{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.CombineEpics where

import Control.Arrow (first)
import Data.List (sortOn)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util
import Sound.Tidal.Epic.Transform


-- | The duration of `eStack a b` is the LCM of the durations of `a` and `b`.
eStack :: Epic a -> Epic a -> Epic a
eStack (Epic df f) (Epic dg g) = Epic (lcmRatios <$> df <*> dg) $
  \arc -> sortOn (fst . fst) $ f arc ++ g arc
  -- sort by first element because takeOverlappingEvs needs that

-- | Only repeating Epics are concatenable.
-- TODO: handle finite-duration non-repeating Epics too.
concatEpic :: Epic a -> Epic a -> Epic a
concatEpic (Epic Nothing _) _ = error "concatEpic requires repeating patterns"
concatEpic _ (Epic Nothing _) = error "concatEpic requires repeating patterns"
concatEpic e1@(Epic (Just t1) f1) e2@(Epic (Just t2) f2) =
  loope (t1 + t2) $ Epic (Just $ t1 + t2) $ \arc -> f1 arc ++ f2 arc
  -- earlier I used eStack, but that takes the LCM of their durations
  where (Epic _ f1) = window (0,t1) e1
        (Epic _ f2) = late t1 $ window (0,t2) e2

-- | "Breathe" a loop of length smallDur to cover a loop of length bigDur.
-- First play the loop, then silence; repeat at next multiple of bigDur.
-- Purpose: if an epic has a glue-duration distinct from its true duration,
-- e.g. thanks to "sparse", concat will still work as expected.
-- Example: Imagine breathing an epic E of length S=2 across a length B=5.
-- The interval (0,2) is unchanged from E.
-- The interval (2,5) is silent.
-- The interval (5,7) carries what (2,4) carries under E.
-- The interval (7,10) is silent ...
--
-- ASSUMES b > ed. You can breathe a small loop into a bigger interval;
-- the reverse doesn't make obvious sense.

breathe :: Time -> Epic a -> Epic a
breathe bigDur (Epic Nothing _) = error "Breathe is only defined for loops."
breathe bigDur (Epic (Just smallDur) ef) = Epic (Just bigDur) $ \(s,e) ->
  let covered = breathAddGaps bigDur smallDur (s,e)
        -- the non-gaps, the intervals that will carry payloads
      contracted = map (breathContract bigDur smallDur) covered
        -- the inner intervals, on which ef is computed
  in map (first $ breathExpand bigDur smallDur)
     $ concatMap ef contracted

breathAddGaps :: Time -> Time -> Arc -> [Arc]
breathAddGaps big small (s,e) =
  if s >= e -- todo ? does this violate the idiom established by `overlap`?
  then []
  else let z = roundDownTo big s -- the first phase 0 before (or equal to) s
           z' = z + big -- the next phase 0
           endOne = z + small -- the end of the first covered interval
           ov = overlap (z,endOne) (s,e)
       in case ov of Nothing -> [] -- seems impossible
                     Just x -> x : breathAddGaps big small (z',e)

breathContract :: Time -> Time -> Arc -> Arc
breathContract big small (s,e) = let s' = small * fromIntegral (div' s big)
                                 in (s', s' + e-s)

breathExpand :: Time -> Time -> Arc -> Arc
breathExpand big small (s,e) = let n = fromIntegral $ div' s small
                                   r = rem' s small
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

applyMetaEpic :: Epic (Epic a -> Epic b) -> Epic a -> Epic b
applyMetaEpic    (Epic md mf)               obj    = Epic d' f' where
  d' = lcmRatios <$> md <*> period obj
  -- f' :: Arc -> [Ev b]
  f' a = concatMap h transformEvs
    where -- transformEvs :: [(Arc, Epic a -> Epic b)]
          transformEvs = mf a
          -- h :: (Arc, Epic a -> Epic b) -> [Ev b]
          h (arc,tr) = eArc (tr obj) arc
