{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Phoneme.Number (epicPhonemeDouble, epicPhonemeRatio) where

import           Control.Applicative
import qualified Data.Map                   as M
import           Data.Ratio
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


-- | = Double
epicPhonemeDouble :: Parser (EpicPhoneme (Maybe Double))
epicPhonemeDouble = foldl1 (<|>)
  [epicPhonemePersistDouble, epicPhonemeFor, epicPhonemeSilence]
epicPhonemePersistDouble = EpicPhonemeNewPersist <$> pDouble

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicPhonemeFor, epicPhonemeSilence :: Parser (EpicPhoneme a)
epicPhonemeFor = lexeme $ ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicPhonemeSilence = lexeme $ const EpicPhonemeSilent <$> char '_'

pDouble :: Parser (Maybe Double)
pDouble = Just <$> double


-- | = Ratio
epicPhonemeRatio :: Integral a => Parser (EpicPhoneme (Maybe (Ratio a)))
epicPhonemeRatio = foldl1 (<|>)
  [epicPhonemePersistRatio, epicPhonemeFor', epicPhonemeSilence']
epicPhonemePersistRatio :: Integral a => Parser (EpicPhoneme (Maybe (Ratio a)))
epicPhonemePersistRatio = lexeme $ EpicPhonemeNewPersist <$> pRatio

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicPhonemeFor', epicPhonemeSilence' :: forall a. Parser (EpicPhoneme a)
epicPhonemeFor' = lexeme $ ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicPhonemeSilence' = lexeme $ const EpicPhonemeSilent <$> char '_'

pRatio :: Integral a => Parser (Maybe (Ratio a))
pRatio = Just <$> ratio
