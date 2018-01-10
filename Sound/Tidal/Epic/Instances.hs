module Sound.Tidal.Epic.Instances where

import Control.Arrow (first, second)
import Control.Applicative
import Data.Map (singleton)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Time (mapArc)

import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Util (takeOverlappingEvs, lcmRatios)
import Sound.Tidal.Epic.Transform (ever)


instance Functor Epic where
  fmap f (Epic d e) = Epic d $ g e
    where g = fmap $ fmap $ \(arc, a) -> (arc, f a)

instance Applicative Epic where
  pure = ever -- ^ anything else would violate an Applicative law
  (<*>) = applyEpic

applyEpic :: Epic (x -> y) -> Epic x -> Epic y
applyEpic (Epic df ef) (Epic dx ex) = Epic dr er -- func f, obj x, result r
  where dr = lcmRatios <$> df <*> dx
        er (s,e) = k (ef (s,e)) (ex (s,e))
        k :: [Ev (x->y)] -> [Ev x] -> [Ev y]
        k [] _ = []
        k _ [] = []
        k ((arc,f):fs) xs =
          let overlapping = takeOverlappingEvs arc xs
              remaining = dropWhile ((< fst arc) . snd . fst) xs
                -- drop xs that end before arc starts
          in fmap (second f) overlapping ++ k fs remaining

instance (Fractional a) => Fractional (Epic a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance Num a => Num (Epic a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum
