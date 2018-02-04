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
  let e1' =           space (t1+t2) e1
      e2' = late t1 $ space (t1+t2) e2
  in stack e1' e2'

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
