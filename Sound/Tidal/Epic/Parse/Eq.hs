{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , StandaloneDeriving
#-}

-- WARNING: The Eq instances are unlawful. If an epic has a period less than
-- or equal to 10, this will I believe always correctly judge equality.
-- A similar but harder to state condition applies to operators.

-- The Eq instances are only used in the test suite.
-- The Ord instance is only used in Parse.Lexeme

module Sound.Tidal.Epic.Parse.Eq where


import Data.Map (Map(..),singleton)

import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Parse.Types

import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Params


instance Eq Scale where
  (==) a b = let testList = map (singleton deg_p . VF) [1..10]
                 at = a <$> testList
                 bt = b <$> testList
             in at == bt

instance Ord Scale where
  (<=) a b = let testList = map (singleton deg_p . VF) [1..10]
                 at = a <$> testList
                 bt = b <$> testList
             in at <= bt

deriving instance (Eq i, TestEpic i) => Eq (NonEpicLexeme i)
deriving instance (Eq o, Eq (NonEpicLexeme i)) => Eq (Lexeme i o)
deriving instance (Eq i, Eq o, TestEpic i) => Eq (Lang i o)

mkTestEpic :: a -> a -> Epic a
mkTestEpic a b = concatEpic f g where
  f = eStack (loopa 1 a) (loopa 2 b)
  g = loopa 3 b

class TestEpic a where
  testEpic :: Epic a

instance TestEpic Int where testEpic = mkTestEpic 1 2
instance TestEpic Float where testEpic = mkTestEpic 1 2

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
