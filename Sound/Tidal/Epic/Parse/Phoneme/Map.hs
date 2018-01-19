module Sound.Tidal.Epic.Parse.Phoneme.Map (MSD, epicPhoneme) where

import           Control.Applicative
import qualified Data.Map                   as M
import           Data.Void (Void)
import           GHC.Exts( IsString(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char (string, char)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Parse.Types

import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Util (
  Parser(..), anyWord, anyDigitlessWord, double, ignore, ratio)


type MSD = M.Map String Double

-- | = Boilerplate, common to Scales, (Epic ParamMap) and eventually
-- (Epic (Map String Value))
epicPhoneme, epicPhonemePersist, epicPhonemeOnce :: Parser (EpicPhoneme MSD)
epicPhoneme = foldl1 (<|>) [epicPhonemePersist, epicPhonemeOnce, epicLexemfor, epicLexemsilence]
epicPhonemePersist = EpicPhonemeNewPersist <$> pSingleton
epicPhonemeOnce = EpicPhonemeOnce <$> (ignore (char '1') >> pSingleton)

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicLexemfor, epicLexemsilence :: Parser (EpicPhoneme a)
epicLexemfor = ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicLexemsilence = const EpicPhonemeSilent <$> char '_'

pSingleton :: Parser MSD
pSingleton = M.singleton <$> anyDigitlessWord <*> double
