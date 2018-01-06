{-# LANGUAGE FlexibleInstances #-}

module Sound.Tidal.Epic.Parse.Eq where

import Data.Map (Map(..),singleton)

import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Parse.Types

import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Params


mkTestEpic :: a -> a -> Epic a
mkTestEpic a b = concatEpic f g where
  f = eStack (loopa 1 a) (loopa 2 b)
  g = loopa 3 b

class TestEpic a where
  testEpic :: Epic a

instance TestEpic (Map Param Value) where
  testEpic = mkTestEpic (singleton sound_p $ VS "bd")
             (singleton gain_p $ VF 2)

instance Eq a => Eq (Epic a) where
  (==) a b = let arc = (0,10) in eArc a arc == eArc b arc

instance (Eq a, TestEpic a) => Eq (Epic a -> Epic a) where
  (==) f g = let t = testEpic
                 arc = (0,10)
             in eArc (f t) arc == eArc (g t) arc

instance (Eq a, TestEpic a) => Eq (Epic a -> Epic a -> Epic a) where
  (==) f g = let t1 = testEpic
                 t2 = early 1 $ slow 2 testEpic
                 arc = (0,10)
             in eArc (f t1 t2) arc == eArc (g t1 t2) arc
