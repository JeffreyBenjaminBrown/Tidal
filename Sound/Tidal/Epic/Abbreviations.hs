-- | == These abbreviations conflict with a lot of Tidal's names.
-- I load them with ":m +Sound.Tidal.Epic", which avoids those conflicts
-- by not importing the rest of Tidal.

module Sound.Tidal.Epic.Abbreviations (
  -- | ==== from Prelude or Data.List
  repli
  , pr

  -- | ==== Tidal
  , arc
  , shh
  , fast, slow
  , rev
  , tog
  , for

  -- | == concatenation
  , cat, cata, cat0
  , loope, loopa, loop0
  , (<**>)
  , (&+), (&*)
  , (+-), (+|)

  -- | == Params
  , syf
  ) where

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Scales
import Sound.Tidal.Stream (mergeNumWith)


repli = replicate
pr :: Show a => [a] -> IO ()
pr = mapM_ (putStrLn . show)

infixl 4 <**>
(<**>) = applyMetaEpic
infixr 3 &*, &+
(&+) = mergeEpics (+) (+)
(&*) = mergeEpics (*) (*)
infixr 2 +-, +|
(+-) = concatEpic
(+|) = eStack

arc = eArc
shh = eSilence
dsh = durSilence
fast = eFast
slow = eSlow
rev = eRev
tog = eStack
for = eDur

-- | == concatenation
cat :: [Epic a] -> Epic a
cat = foldl1 (+-)
cata, cat0 :: Time -> [a] -> Epic a
cata t = foldl1 (+-) . map (loopa t)
cat0 t = foldl1 (+-) . map (loop0 t)

loope        :: Time -> Epic a -> Epic a
loope = eRepeat
loopa, loop0 :: Time -> a      -> Epic a
loopa dur = eRepeat dur . eDur dur
loop0 dur = eRepeat dur . eDur 0

syf = syFreq
