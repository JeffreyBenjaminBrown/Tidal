module Sound.Tidal.Epic.Parse.Scale
  -- (epicLexeme)
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

import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Util (
  Parser(..), anyWord, double, ignore, lexeme, ratio)
import Sound.Tidal.Epic.Scale


-- | = Boilerplate, common to Scales, (Epic ParamMap) and eventually
-- (Epic (Map String Value))
epicLexeme, epicLexemePersist, epicLexemeOnce ::
  Parser (EpicLexeme (Maybe Scale))
epicLexeme = foldl1 (<|>)
  [epicLexemePersist, epicLexemeOnce, epicLexemeDur, epicLexemeSilence]
epicLexemePersist = lexeme $ EpicLexemeNewPersist <$> pScale
epicLexemeOnce = lexeme $ EpicLexemeOnce <$> (ignore (char '1') >> pScale)

-- >> TODO: make these universal, not just for ParamMaps but scales, etc.
epicLexemeDur, epicLexemeSilence :: Parser (EpicLexeme a)
epicLexemeDur = lexeme $ ignore (char 't') >> EpicLexemeDur <$> ratio
  -- >> todo ? accept floats as well as ratios
epicLexemeSilence = lexeme $ const EpicLexemeSilent <$> char '_'


-- | = Scale-specific
scales = [
  dim,   dimd,   aug,   augd,   hol
  ,   maj,   dor,   phr,   lyd,   mix,   aol,   loc
  ,   maj5,   dor4,   phr3,   lyd2,   loc47,   aol7,   loc6
  ,   maj6,   dor5,   phr4,   lyd3,   mix2,   lyd25,   loc7
  ,   maj3,   dor7,   dor2,   phr6,   lyd5,   lyd7,   mix4,   mix6
    ,   aol3,   aol5,   loc2,   loc4
  ] :: [ParamMap -> ParamMap]

scaleNames = [
  "dim", "dimd", "aug", "augd", "hol"
  , "maj", "dor", "phr", "lyd", "mix", "aol", "loc"
  , "maj5", "dor4", "phr3", "lyd2", "loc47", "aol7", "loc6"
  , "maj6", "dor5", "phr4", "lyd3", "mix2", "lyd25", "loc7"
  , "maj3", "dor7", "dor2", "phr6", "lyd5", "lyd7", "mix4", "mix6"
    , "aol3", "aol5", "loc2", "loc4"
  ] :: [String]

pScale :: Parser (Maybe Scale)
pScale = foldl1 (<|>) _individualScaleParsers where
  _individualScaleParsers :: [Parser (Maybe Scale)]
  _individualScaleParsers = map f $ zip scales scaleNames
    where f (scale,name) = lexeme $ string name >> return (Just scale)
