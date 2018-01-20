module Sound.Tidal.Epic.Types where

import Control.Lens
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

type Scale = ParamMap -> ParamMap
