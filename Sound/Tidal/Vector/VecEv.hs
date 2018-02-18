module Sound.Tidal.Vector.VecEv where

import           Data.Maybe (isNothing)
import qualified Data.Vector as V

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types
import Sound.Tidal.Vector.Util


support :: VecEv a -> Arc
support ve = (veStart ve, veStart ve + veDuration ve)

overlapOfEv :: Arc -> VecEv a -> Maybe Arc
overlapOfEv arc ve = overlap arc $ support ve

overlapsEv :: Arc -> VecEv a -> Bool
overlapsEv arc ve = if isNothing $ overlapOfEv arc ve then False else True

-- | ASSUMES nonzero overlap
clip :: Arc -> VecEv a -> VecEv a
clip arc ve = let Just (a,b) = overlapOfEv arc ve
  in ve {veStart = a, veDuration = b-a}
