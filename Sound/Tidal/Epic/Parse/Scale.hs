module Sound.Tidal.Epic.Parse.Scale where

import           Text.Megaparsec
import           Text.Megaparsec.Char (string)

import           Sound.Tidal.Epic.Types.Reimports

import           Sound.Tidal.Epic.Scale
import           Sound.Tidal.Epic.Parse.Util (Parser(..), lexeme)


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

pScale :: Parser (ParamMap -> ParamMap)
pScale = foldl1 (<|>) _individualScaleParsers where
  _individualScaleParsers :: [Parser (ParamMap -> ParamMap)]
  _individualScaleParsers = map f $ zip scales scaleNames
    where f (scale,name) = lexeme $ string name >> return scale
