{-# LANGUAGE TupleSections, ScopedTypeVariables #-}

module Sound.Tidal.Vector.Util where

import Control.Arrow (first)
import Data.Fixed (div')
import Data.List (sortOn, partition)
import Data.List.Unique (sortUniq)
import qualified Data.Map as M
import Data.Ratio
import qualified Data.Set as S

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types


(<*<) :: Applicative f => f (a -> b) -> f a -> f b
(<*<) = (<*>)
infixr 4 <*<

(<$<) :: Applicative f => (a -> b) -> f a -> f b
(<$<) = (<$>)
infixr 4 <$<
  
plist :: Show a => [a] -> IO ()
plist = mapM_ (putStrLn . show)

composeMaps :: (Ord k, Ord r) => M.Map k r -> M.Map k a -> M.Map r a
composeMaps f m = let s = S.fromList $ M.keys f
                      m' = M.restrictKeys m s
                  in M.mapKeys ((M.!) f) m

roundDownTo :: Time -> Time -> Time
roundDownTo den num = den * fromIntegral (div' num den)

-- ^ in a list, do f to what passes, g to what fails the test
toPartitions :: forall a b.
  (a->Bool) -> ([a]->[b]) -> ([a]->[b]) -> [a] -> [b]
toPartitions test f g as =
  let (for_f, for_g) = partition test as
      (did_f, did_g) = (f for_f, g for_g)
      reconstruct :: [a] -> [b] -> [b] -> [b]
      reconstruct [] _ _ = []
      reconstruct (o:originals) did_f did_g = case test o of
        True  -> head did_f : reconstruct originals (tail did_f) did_g
        False -> head did_g : reconstruct originals did_f        (tail did_g)
  in reconstruct as did_f did_g

-- | ASSUMES a <= b and c <= d.
 -- A point can overlap with the start (and not the end)
 -- of an interval, but two intervals sharing a single point have no overlap.
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
  f (s,e) ovs (((s',e'),a):evsRest)
    | s > e'              = f (s,e) ovs evsRest -- discard
    | s == e' && s' < e'  = f (s,e) ovs evsRest -- discard
 -- A point can overlap with the start (and not the end)
 -- of an interval, but two intervals sharing a single point have no overlap.
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
