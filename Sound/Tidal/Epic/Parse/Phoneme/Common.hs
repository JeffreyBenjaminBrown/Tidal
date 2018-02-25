module Sound.Tidal.Epic.Parse.Phoneme.Common where

import Text.Megaparsec
import Text.Megaparsec.Char (string, char)
import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Parse.Types
import Sound.Tidal.Epic.Parse.Util (Parser(..), ignore, ratio)


epicPhonemeFor, epicPhonemeSilence :: Parser (EpicPhoneme a)
epicPhonemeFor = ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicPhonemeSilence = const EpicPhonemeSilent <$> char '_'
