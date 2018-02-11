-- Needs work: &* evalautes the continuous epic only at its start,
-- rather than at each change of the discrete epic it's merged with.
--
-- > plist $ _arc (gain sine &* pe0 "*8 _bd") (0,1)
-- ((0 % 1,0 % 1),fromList [(gain,0.5),(s,bd)])
-- ((1 % 8,1 % 8),fromList [(gain,0.5),(s,bd)])
-- ((1 % 4,1 % 4),fromList [(gain,0.5),(s,bd)])
-- ((3 % 8,3 % 8),fromList [(gain,0.5),(s,bd)])
-- ((1 % 2,1 % 2),fromList [(gain,0.5),(s,bd)])
-- ((5 % 8,5 % 8),fromList [(gain,0.5),(s,bd)])
-- ((3 % 4,3 % 4),fromList [(gain,0.5),(s,bd)])
-- ((7 % 8,7 % 8),fromList [(gain,0.5),(s,bd)])

module Sound.Tidal.Epic.Waveforms where

import Control.Lens ((.~))
import Data.Fixed (mod')
import GHC.Real (toRational)

import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.CombineEpics (append)
import Sound.Tidal.Epic.Transform (fast, rev)


sig :: (Time -> a) -> Epic a
sig f = Epic Nothing $ \(s,e) -> if s > e then [] else [((s,e), f s)]

sine2 :: Epic Double
sine2 = period .~ Just 1
  $ sig $ sin . (* (2 * pi)) . fromRational

sine :: Epic Double
sine = (+(1/2)) . (/2) $ sine2

saw :: Epic Double
saw = period .~ Just 1
  $ sig $ \t -> mod' (fromRational t) 1

sawFall :: Epic Double
sawFall = period .~ Just 1
  $ sig $ \t -> (+1) . (*(-1)) $ mod' (fromRational t) 1
  -- todo ? sawFall /= rev saw; rev is weird for such "continuous" values

saw2 :: Epic Double
saw2 = (+(-1)) . (*2) <$> saw

sawFall2 :: Epic Double
sawFall2 = (+(-1)) . (*2) <$> sawFall

tri :: Epic Double
tri = fast 2 $ append saw sawFall

tri2 :: Epic Double
tri2 = (+(-1)) . (*2) <$> tri
