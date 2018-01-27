{-# LANGUAGE TemplateHaskell #-}

module Sound.Tidal.Epic.Types where

import Control.Lens
import Data.Ratio
import Data.Typeable
import Sound.Tidal.Epic.Types.Reimports


type Ev a = (Arc,a) -- like Event, but only one arc

-- | Like a Pattern, but period-aware, and maybe without cycling
data Epic a = Epic { _period :: Maybe Time -- ^ Nothing if not repeating
                   , _arc :: Arc -> [(Arc,a)]
  -- AMBITION ? add field support :: (Maybe Time, Maybe Time)
  -- or offset :: Maybe Time and duration :: Maybe Time
  -- (where snd <$> support = (+) <$> offset <*> duration)
  -- Currently period is meaningful for repeating Epics, not others.
                   } deriving Typeable
makeLenses ''Epic

type ParamEpic = Epic ParamMap

-- | A scale changes deg_p values to speed_p values.
-- If it involves transposition, deg_p = 0 maps to some speed_p /= 1
-- todo ? make non-scales (e.g. gain transformations) invalid
  -- that is, constrain Scales to only be maps from deg_p to speed_p
type Scale = ParamMap -> ParamMap

data Harmony = Harmony {
  baseScale :: Scale -- ^ a Scale without transposition
  , root :: Rational -- ^ a relative frequency change
  , scaleSize :: Int -- ^ how many notes are in the scale
  , chord :: [Int] -- ^ scale degrees, in [0 .. length-1], maybe all of them
  }

data ScoreRule =
  InScale Int -- ^ penalty if not in scale
  | InChord Int -- ^ penalty if not in chord
  | Unique Int -- ^ penalty if equal to another voice
    -- todo ? make degree-specific. e.g. doubling the root might be okay.
  | DegDiff Int -- ^ if corrected degree = x and original = y,
    -- penalize by (x-y)*this
