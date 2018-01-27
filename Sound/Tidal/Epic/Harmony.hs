module Sound.Tidal.Epic.Harmony where

import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Instances
import           Sound.Tidal.Epic.Scale
import           Sound.Tidal.Epic.Params

scaleFromHarmony :: Harmony -> Scale
scaleFromHarmony h = transposeScale (fromRational $ root h) (baseScale h)
