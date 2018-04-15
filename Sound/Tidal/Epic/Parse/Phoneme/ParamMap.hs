-- Pitfall: With OverloadedStrings, `parse` needs a type signature
-- to specify that the last argument is a String.

module Sound.Tidal.Epic.Parse.Phoneme.ParamMap
  (epicPhoneme, pSingleton)
where

import           Control.Applicative
import qualified Data.Map                   as M
import           Data.Void (Void)
import           GHC.Exts( IsString(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char (string, char)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Parse.Types
import Sound.Tidal.Epic.Parse.Phoneme.Common

import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Util (
  Parser(..), anyWord, double, ignore, ratio)


-- | = Boilerplate, common to Scales, (Epic ParamMap) and eventually
-- (Epic (Map String Value))
epicPhoneme, epicPhonemePersist, epicPhonemeOnce :: Parser (EpicPhoneme ParamMap)
epicPhoneme = foldl1 (<|>) [epicPhonemePersist, epicPhonemeOnce, epicPhonemeFor, epicPhonemeSilence]
epicPhonemePersist = EpicPhonemeNewPersist <$> pSingleton
epicPhonemeOnce = EpicPhonemeOnce <$> (ignore (char '1') >> pSingleton)


-- | = (Epic ParamMap)-specific
pSingleton :: Parser ParamMap
pSingleton = foldl1 (<|>) $ map try [parseSpeed, parseSpeedr, parseGain
  , parseSound, parseSample, parseDegree, parseSustain, parsePan, parseShape
  , parseQfa, parseQff, parseQffabs, parseQpa, parseQpf, parseQaa, parseQaf
  , parseQfr, parseQf, parseAmp ]

parseSpeed, parseSpeedr, parseGain, parseSound, parseSample, parseDegree, parseSustain, parsePan, parseShape, parseQfa, parseQff, parseQffabs, parseQpa, parseQpf, parseQaa, parseQaf, parseQf, parseQfr, parseAmp :: Parser ParamMap
parseSpeed   = pSingletonFloat  speed_p            $ ignore $ char 's'
parseSpeedr  = pSingletonFloatFromRational speed_p $ ignore $ string "sr"
parseGain    = pSingletonFloat  gain_p             $ ignore $ char 'g'
parseSound   = pSingletonString sound_p            $ ignore $ char '_'
parseSample  = pSingletonString sample_p           $ ignore $ char ':'
parseDegree  = pSingletonFloat  deg_p              $ ignore $ char 'd'
parseSustain = pSingletonFloat  sustain_p          $ ignore $ string "sus"
parsePan     = pSingletonFloat  pan_p              $ ignore $ string "pan"
parseShape   = pSingletonFloat  shape_p            $ ignore $ string "shape"
  -- "shape" = distortion
parseQfa     = pSingletonFloat  qfa_p              $ ignore $ string "fa"
parseQff     = pSingletonFloat  qff_p              $ ignore $ string "ff"
parseQffabs  = pSingletonFloat  qffabs_p           $ ignore $ string "ffabs"
parseQpa     = pSingletonFloat  qpa_p              $ ignore $ string "pa"
parseQpf     = pSingletonFloat  qpf_p              $ ignore $ string "pf"
parseQaa     = pSingletonFloat  qaa_p              $ ignore $ string "aa"
parseQaf     = pSingletonFloat  qaf_p              $ ignore $ string "af"
parseQfr     = pSingletonFloatFromRational  qf_p   $ ignore $ string "fr"
parseQf      = pSingletonFloat  qf_p               $ ignore $ string "f"
parseAmp     = pSingletonFloat              qa_p   $ ignore $ string "a"

pSingletonFloat :: Param -> Parser () -> Parser ParamMap
pSingletonFloat param prefix =
  M.singleton param . VF <$> (prefix >> double)

pSingletonFloatFromRational :: Param -> Parser () -> Parser ParamMap
pSingletonFloatFromRational param prefix =
  M.singleton param . VF . fromRational <$> (prefix >> ratio)

pSingletonString :: Param -> Parser () -> Parser ParamMap
pSingletonString param prefix =
  M.singleton param . VS <$> (prefix >> anyWord)
