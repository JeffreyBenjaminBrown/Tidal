module Sound.Tidal.Epic.Parse.Phoneme.Scale (epicPhoneme) where

import           Control.Applicative
import qualified Data.Map                   as M
import           Data.Void (Void)
import           GHC.Exts( IsString(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char (string, char)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Parse.Types

import Sound.Tidal.Epic.Util (mergeNumParamsWith)
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Util (
  Parser(..), anyWord, double, ignore, lexeme, ratio)
import Sound.Tidal.Epic.Scale


-- | = Boilerplate, common to Scales, (Epic ParamMap) and eventually
-- (Epic (Map String Value))
epicPhoneme, epicPhonemePersist ::
  Parser (EpicPhoneme (Maybe Scale))
epicPhoneme = foldl1 (<|>)
  [epicPhonemePersist, epicPhonemeFor, epicPhonemeSilence]
epicPhonemePersist = EpicPhonemeNewPersist <$> pMSWR

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicPhonemeFor, epicPhonemeSilence :: Parser (EpicPhoneme a)
epicPhonemeFor = lexeme $ ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicPhonemeSilence = lexeme $ const EpicPhonemeSilent <$> char '_'


-- | = Scale-specific

-- WARNING: Hack: To prevent something like "phr3" from parsing as "phr",
-- for any two scales x, y such that y is a prefix of x, locate y second.
scales = [
  maj5,   dor4,   phr3,   lyd2,   loc47,   aol7,   loc6
  ,   maj6,   dor5,   phr4,   lyd3,   mix2,   lyd25,   loc7
  ,   maj3,   dor7,   dor2,   phr6,   lyd5,   lyd7,   mix4,   mix6
    ,   aol3,   aol5,   loc2,   loc4
  , dimd,   dim,   augd,   aug,   hol
  ,   maj,   dor,   phr,   lyd,   mix,   aol,   loc
  ] :: [ParamMap -> ParamMap]

scaleNames = [
  "maj5", "dor4", "phr3", "lyd2", "loc47", "aol7", "loc6"
  , "maj6", "dor5", "phr4", "lyd3", "mix2", "lyd25", "loc7"
  , "maj3", "dor7", "dor2", "phr6", "lyd5", "lyd7", "mix4", "mix6"
    , "aol3", "aol5", "loc2", "loc4"
  , "dimd", "dim", "augd", "aug", "hol"
  , "maj", "dor", "phr", "lyd", "mix", "aol", "loc"
  ] :: [String]

pScale :: Parser Scale
pScale = foldl1 (<|>) _individualScaleParsers where
  _individualScaleParsers :: [Parser Scale]
  _individualScaleParsers = map f $ zip scales scaleNames
    where f (scale,name) = string name >> return scale

pScaleWithRoot :: Parser Scale
pScaleWithRoot = transposedScale <|> pScale where
  transposedScale = 
    do r <- (\n -> 2**(n/12)) <$> double
       s <- pScale
       let transpose = mergeNumParamsWith (*) (*) (M.singleton speed_p $ VF r)
       return $ transpose . s -- transpose second, else might have no speeds

pMSWR :: Parser (Maybe Scale)
pMSWR = Just <$> pScaleWithRoot
