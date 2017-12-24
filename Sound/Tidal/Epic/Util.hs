{-# LANGUAGE TupleSections #-}

module Sound.Tidal.Epic.Util where

import Control.Arrow (first)
import Data.List (sortOn)
import Data.List.Unique (sortUniq)
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Set as S

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types


-- | ASSUMES a <= b and c <= d.
overlap :: Arc -> Arc -> Maybe Arc
overlap (a,b) (c,d) | b <  c || d < a = Nothing
                    | b == c && a < b = Nothing
                    | d == a && c < d = Nothing
                    | otherwise = Just (max a c, min b d)

-- | ASSUMES the events are sorted by start time.
-- (Can't assume they don't overlap, because they might be polyphonic.)
takeOverlappingEvs :: Arc -> [Ev a] -> [Ev a]
takeOverlappingEvs (s,e) evs = reverse $ f (s,e) [] evs where
  f :: Arc ->   [Ev a]   ->    [Ev a]  -> [Ev a]
  f     _        ovs           []      =  ovs
  f (s,e) ovs (e1@((s',e'),a):evsRest)
    | s >  e'   = f (s,e) ovs evsRest -- the only way discard happens
    | otherwise = case overlap (s,e) (s',e') of
        Nothing    -> ovs
        Just ovArc -> f (s,e) ((ovArc,a):ovs) evsRest

-- | Produces a sorted list of nonoverlapping events.
partitionAndGroupEvents :: [Ev a] -> [Ev a]
partitionAndGroupEvents evs =
  partitionAndGroupEventsAtBoundaries (boundaries $ map fst evs) evs

partitionAndGroupEventsAtBoundaries :: [Time] -> [Ev a] -> [Ev a]
partitionAndGroupEventsAtBoundaries bs evs =
  let partitionEv :: Ev a -> [Ev a]
      partitionEv (arc,x) = map (,x) $ partitionArcAtTimes bs arc
  in sortOn fst $ concatMap partitionEv evs

-- | Assumes the times are sorted and uniqe (ala Data.List.sortUniq),
-- and that they include the endpoints of `arc`.
partitionArcAtTimes :: [Time] -> Arc -> [Arc]
partitionArcAtTimes (a:b:ts) (c,d)
  | c > a = partitionArcAtTimes (b:ts) (c,d)
  | b == d = [(a,b)]
  | otherwise = (a,b) : partitionArcAtTimes (b:ts) (b,d)

-- | Produces a sorted list of arc endpoints.
-- If `arcs` includes `(x,x)`, then `x` will appear twice in the output.
boundaries :: [Arc] -> [Time]
boundaries arcs = _doubleTheDurationZeroBoundaries arcs
  $ sortUniq $ map fst arcs ++ map snd arcs

_doubleTheDurationZeroBoundaries :: [Arc] -> [Time] -> [Time]
_doubleTheDurationZeroBoundaries arcs bounds = concatMap f bounds where
  instants :: S.Set Time
  instants = S.fromList $ map fst $ filter (\(s,e) -> s == e) arcs
  f t = if S.member t instants then [t,t] else [t]

lcmRatios :: Rational -> Rational -> Rational
lcmRatios x y = let (a,b) = (numerator x, denominator x)
                    (c,d) = (numerator y, denominator y)
                    l = lcm b d
                in lcm (a * div l b) (c * div l d) % l

-- | like Sound.Tidal.Stream.mergeNumWith, but outside a functor
mergeNumParamsWith :: (Int -> Int -> Int) -> (Double -> Double ->  Double) ->
  ParamMap -> ParamMap -> ParamMap
mergeNumParamsWith intOp floatOp = M.unionWithKey f where
  f (I _ _) (VI a) (VI b) = VI $ intOp a b
  f (F _ _) (VF a) (VF b) = VF $ floatOp a b
  f _       _      b      = b
