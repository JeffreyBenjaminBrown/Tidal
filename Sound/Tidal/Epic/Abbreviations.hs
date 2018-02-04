-- | == These abbreviations conflict with a lot of Tidal's names.
-- I load them with ":m +Sound.Tidal.Epic", which avoids those conflicts
-- by not importing the rest of Tidal.

module Sound.Tidal.Epic.Abbreviations (
  -- | ==== from Prelude or Data.List
  repli

  -- | ==== Tidal
  , shh, dsh

  -- | == concatenation
  , cat, cata, cat0
  , (<**>)
  , (&+), (&*)
  , (+-), (+|)
  , swing

  -- | == Params
  , syp
  , stackaParamR
  ) where

import Control.Lens ((.~))
import Data.Ratio

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Scale
import Sound.Tidal.Stream (mergeNumWith)


repli = replicate

infixl 4 <**>
(<**>) = meta
infixr 3 &*, &+
(&+) = mergeEpics (+) (+)
(&*) = mergeEpics (*) (*)
infixr 2 +-, +|
(+-) = append
(+|) = stack

shh = silence
dsh = durSilence


-- | == concatenation
cat :: [Epic a] -> Epic a
cat = foldl1 (+-)
cata, cat0 :: Time -> [a] -> Epic a
cata t = foldl1 (+-) . map (loopa t)
cat0 t = foldl1 (+-) . map (loop0 t)

swing :: Time -> Time -> Epic a -> Epic a
swing bigDur smallDur ep = ep' +- dsh (bigDur - smallDur)
  where ep' = period .~ Just smallDur $ fast (bigDur / smallDur) ep


-- | == parameters
syp = syParams

-- | Stack a list of Ratios as parameter values
-- Handy for just intonation: v1 $ tone &* stackaParamR 1 qf_p [1,2,11%4]
stackaParamR :: Time -> Param -> [Ratio Integer] -> Epic ParamMap
stackaParamR t p = stacka t . fmap (remapPd p . fromRational)
