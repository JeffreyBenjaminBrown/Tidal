{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.CombineEpics where

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
  eRepeat (t1 + t2) $ Epic (Just $ t1 + t2) $ \arc -> f1 arc ++ f2 arc
  -- earlier I used eStack, but that takes the LCM of their durations
  where (Epic _ f1) = window (0,t1) e1
        (Epic _ f2) = late t1 $ window (0,t2) e2

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
