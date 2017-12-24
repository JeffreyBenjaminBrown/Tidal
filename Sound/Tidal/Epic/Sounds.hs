module Sound.Tidal.Epic.Sounds where

import Data.Map
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Types.Reimports

import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Types


-- | Deprecated. Noteworthy in that it is impossible.
-- Play a pattern (of pitches, say) through some sound (e.g. violin).
-- The problem: The event shrunk to duration zero starts at the start of `arc`
-- which might be in the middle of the event we intended to shrink to zero.
_onSound :: String -> ParamEpic -> ParamEpic
_onSound s e = Epic d $ \arc -> fmap zeroDur $ f arc
  where Epic d f = fmap (insert sound_p $ VS s) e
        zeroDur ((t,_),e) = ((t,t),e)

-- | == samples
bow = for 0 $ fromList [(sound_p, VS "bow")
                       , (sustain_p, VF 5)]
can = for 0 $ fromList [(sound_p, VS "can")
                       , (sustain_p, VF 5)]
clap = for 0 $ fromList [(sound_p, VS "clap")
                        , (sustain_p, VF 5)]
cow = for 0 $ fromList [(sound_p, VS "cow")
                       , (sustain_p, VF 5)]
crash = for 0 $ fromList [(sound_p, VS "crash")
                         , (sustain_p, VF 5)]
crow = for 0 $ fromList [(sound_p, VS "crow")
                        , (sustain_p, VF 5)]

hatc = for 0 $ fromList [(sound_p, VS "hatc")
                        , (sustain_p, VF 5)]
hato = for 0 $ fromList [(sound_p, VS "hato")
                        , (sustain_p, VF 5)]
hit = for 0 $ fromList [(sound_p, VS "hit")
                       , (sustain_p, VF 5)]

kick = for 0 $ fromList [(sound_p, VS "kick")
                        , (sustain_p, VF 5), (sample_p, VI 1)]
kickb = for 0 $ fromList [(sound_p, VS "kick")
                         , (sustain_p, VF 5)]
kickg = for 0 $ fromList [(sound_p, VS "kick")
                         , (sustain_p, VF 5), (sample_p, VI 2)]
kickn = for 0 $ fromList [(sound_p, VS "kick")
                         , (sustain_p, VF 5), (sample_p, VI 3)]
kickr = for 0 $ fromList [(sound_p, VS "kick")
                         , (sustain_p, VF 5), (sample_p, VI 4)]
kickt = for 0 $ fromList [(sound_p, VS "kick")
                         , (sustain_p, VF 5), (sample_p, VI 5)]

snare = for 0 $ fromList [(sound_p, VS "snare")
                         , (sustain_p, VF 5), (sample_p, VI 1)]
snarea = for 0 $ fromList [(sound_p, VS "snare")
                          , (sustain_p, VF 5), (sample_p, VI 0)]
snaren = for 0 $ fromList [(sound_p, VS "snare")
                          , (sustain_p, VF 5), (sample_p, VI 2)]

toml = for 0 $ fromList [(sound_p, VS "tom")
                        , (sustain_p, VF 5), (sample_p, VI 0)]
tom = for 0 $ fromList [(sound_p, VS "tom")
                       , (sustain_p, VF 5), (sample_p, VI 1)]
tomh = for 0 $ fromList [(sound_p, VS "tom")
                        , (sustain_p, VF 5), (sample_p, VI 2)]
