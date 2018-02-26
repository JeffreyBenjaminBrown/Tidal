-- | based on (and uses) functions in Sound.Tidal.Epic.Parse.Lexeme

{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Phoneme.DJ (epicPhoneme, epicPhonemePm) where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char (char)

import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Parse.Types
import Sound.Tidal.Epic.Parse.Util
import Sound.Tidal.Epic.Parse.Phoneme.Transform (pTransform, pTransformPm)
import Sound.Tidal.Epic.Parse.Phoneme.Common


-- | = parsing a `TWT ParamMap`
epicPhonemePm = foldl1 (<|>) 
  [epicPhonemeOncePm, epicPhonemeFor, epicPhonemeSilence]

epicPhonemeOncePm :: Parser (EpicPhoneme (TWT ParamMap))
epicPhonemeOncePm = pTwtTransformPm <|> pTwtTarget

pTwtTransformPm :: Parser (EpicPhoneme (TWT ParamMap))
pTwtTransformPm = do
  tr <- pTransformPm
  return $ EpicPhonemeOnce $ TWT {twtTargets = S.empty, twtTransform = tr}


-- | = parsing a general `TWT a`
epicPhoneme = foldl1 (<|>) 
  [epicPhonemeOnce, epicPhonemeFor, epicPhonemeSilence]

epicPhonemeOnce :: Parser (EpicPhoneme (TWT a))
epicPhonemeOnce = pTwtTransform <|> pTwtTarget

pTwtTransform :: Parser (EpicPhoneme (TWT a))
pTwtTransform = do
  tr <- pTransform
  return $ EpicPhonemeOnce $ TWT {twtTargets = S.empty, twtTransform = tr}


-- | = common to both `TWT a` and `TWT ParamMap` parsers
pTwtTarget :: Parser (EpicPhoneme (TWT a))
pTwtTarget = do target <- char '@' >> anyWord
                return $ EpicPhonemeOnce $ TWT
                  { twtTargets = S.fromList [target]
                  , twtTransform = id}
