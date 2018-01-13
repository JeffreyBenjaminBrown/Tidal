-- | == These abbreviations conflict with a lot of Tidal's names.
-- I load them with ":m +Sound.Tidal.Epic", which avoids those conflicts
-- by not importing the rest of Tidal.

module Sound.Tidal.Epic.Abbreviations (
  -- | ==== from Prelude or Data.List
  repli
  , pr

  -- | ==== Tidal
  , shh, dsh
  , fast, slow
  , rev
  , tog
  , for

  -- | == concatenation
  , cat, cata, cat0
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
import Sound.Tidal.Epic.Scale
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

syf = syParams
