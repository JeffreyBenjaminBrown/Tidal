module Sound.Tidal.Epic.Types where

import Data.Typeable
import Sound.Tidal.Epic.Types.Reimports


type Ev a = (Arc,a) -- like Event, but only one arc

-- | Like a Pattern, but period-aware, and maybe without cycling
data Epic a = Epic {period :: Maybe Time -- ^ Nothing if not repeating
                   , arc :: Arc -> [(Arc,a)]
  -- AMBITION ? add field support :: (Maybe Time, Maybe Time)
  -- or offset :: Maybe Time and duration :: Maybe Time
  -- (where snd <$> support = (+) <$> offset <*> duration)
  -- Currently period is meaningful for repeating Epics, not others.
                   } deriving Typeable
type ParamEpic = Epic ParamMap

type Scale = ParamMap -> ParamMap
