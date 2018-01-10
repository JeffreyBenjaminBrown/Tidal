-- Pitfall: With OverloadedStrings, `parse` needs a type signature
-- to specify that the last argument is a String.

module Sound.Tidal.Epic.Parse.ParamMap (epicLexeme) where

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
  Parser(..), anyWord, double, ignore, lexeme, ratio)


-- | = Boilerplate, common to Scales, (Epic ParamMap) and eventually
-- (Epic (Map String Value))
epicLexeme, epicLexemePersist, epicLexemeOnce ::
  Parser (EpicLexeme ParamMap)
epicLexeme = foldl1 (<|>) [epicLexemePersist, epicLexemeOnce, epicLexemeDur, epicLexemeSilence]
epicLexemePersist = lexeme $ EpicLexemeNewPersist <$> pSingleton
epicLexemeOnce = lexeme $ EpicLexemeOnce <$> (ignore (char '1') >> pSingleton)

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicLexemeDur, epicLexemeSilence :: Parser (EpicLexeme a)
epicLexemeDur = lexeme $ ignore (char 't') >> EpicLexemeDur <$> ratio
  -- >> todo ? accept floats as well as ratios
epicLexemeSilence = lexeme $ const EpicLexemeSilent <$> char '_'


-- | = (Epic ParamMap)-specific
pSingleton :: Parser ParamMap
pSingleton = foldl1 (<|>) $ map try
  [parseSpeed, parseGain, parseSound, parseDegree, parseSustain
  , parseQf, parseQfa, parseQff, parseQpa, parseQpf, parseQaa, parseQaf ]

parseSpeed, parseGain, parseSound, parseDegree :: Parser ParamMap
parseSpeed   = parseSingletonFloat  speed_p   $ ignore $ char 's'
parseGain    = parseSingletonFloat  gain_p    $ ignore $ char 'g'
parseSound   = parseSingletonString sound_p   $ ignore $ char '_'
parseDegree  = parseSingletonFloat  deg_p     $ ignore $ char 'd'
parseSustain = parseSingletonFloat  sustain_p $ ignore $ string "sus"
parseQf      = parseSingletonFloat  qf_p      $ ignore $ string "f"
parseQfa     = parseSingletonFloat  qfa_p     $ ignore $ string "fa"
parseQff     = parseSingletonFloat  qff_p     $ ignore $ string "ff"
parseQpa     = parseSingletonFloat  qpa_p     $ ignore $ string "pa"
parseQpf     = parseSingletonFloat  qpf_p     $ ignore $ string "pf"
parseQaa     = parseSingletonFloat  qaa_p     $ ignore $ string "aa"
parseQaf     = parseSingletonFloat  qaf_p     $ ignore $ string "af"

parseSingletonFloat :: Param -> Parser () -> Parser ParamMap
parseSingletonFloat param prefix =
  M.singleton param . VF <$> (prefix >> double)

parseSingletonString :: Param -> Parser () -> Parser ParamMap
parseSingletonString param prefix =
  M.singleton param . VS <$> (prefix >> anyWord)
