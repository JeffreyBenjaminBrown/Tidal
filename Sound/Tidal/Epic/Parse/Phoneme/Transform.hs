-- Pitfall: With OverloadedStrings, `parse` needs a type signature
-- to specify that the last argument is a String.

module Sound.Tidal.Epic.Parse.Phoneme.Transform (epicPhoneme) where

import           Control.Applicative
import qualified Data.Map                   as M
import           Data.Void (Void)
import           GHC.Exts( IsString(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char (string, char)

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Parse.Types

import Sound.Tidal.Epic.Abbreviations (swing, shh, chPeriod)
import Sound.Tidal.Epic.CombineEpics ((&*))
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Parse.Util (
  Parser(..), anyWord, double, ignore, ratio)


-- | = Boilerplate, common to Scales, (Epic Transform a) and eventually
-- (Epic (Map String Value))
epicPhoneme, epicPhonemeOnce :: Parser (EpicPhoneme (Transform a))
epicPhoneme = foldl1 (<|>) [epicPhonemeOnce, epicLexemeFor, epicLexemeSilence]
epicPhonemeOnce = EpicPhonemeOnce <$> pTransform

-- >> todo ? make these universal, not just for ParamMaps but scales, etc.
epicLexemeFor, epicLexemeSilence :: Parser (EpicPhoneme a)
epicLexemeFor = ignore (char 't') >> EpicPhonemeFor <$> ratio
  -- >> todo ? accept floats as well as ratios
epicLexemeSilence = const EpicPhonemeSilent <$> char '_'

-- | = Transform-specific
pTransform :: Parser (Transform a)
pTransform = foldl1 (<|>) $ map try
  [ pId, pRev, pShh
  , pFast, pSlow, pDense, pSparse, pEarly, pLate
  , pChPeriod, pSwing, pSpace, pSpace2]

pId, pRev, pShh
  , pFast, pSlow, pDense, pSparse, pEarly, pLate
  , pChPeriod, pSwing, pSpace, pSpace2 :: Parser (Transform a)
pId =       pNoArgOp      id          $ ignore $ string "id"
pRev =      pNoArgOp      rev         $ ignore $ string "rev"
pShh =      pNoArgOp      (const shh) $ ignore $ string "shh"
pFast =     pOneTimeArgOp fast        $ ignore $ string "fa"
pSlow =     pOneTimeArgOp slow        $ ignore $ string "sl"
pDense =    pOneTimeArgOp dense       $ ignore $ string "de"
pSparse =   pOneTimeArgOp sparse      $ ignore $ string "sp"
pEarly =    pOneTimeArgOp early       $ ignore $ string "ea"
pLate =     pOneTimeArgOp late        $ ignore $ string "la"
pChPeriod = pOneTimeArgOp chPeriod    $ ignore $ string "chp"
pSwing =    pTwoTimeArgsOp swing      $ ignore $ string "sw"
pSpace =    pOneTimeArgOp  space      $ ignore $ string "sp"
pSpace2 =   pTwoTimeArgsOp space2     $ ignore $ string "spp"

pNoArgOp :: Transform a -> Parser () -> Parser (Transform a)
pNoArgOp op name = name >> return op

pOneTimeArgOp :: (Time -> Transform a) -> Parser () -> Parser (Transform a)
pOneTimeArgOp op name = op <$> (name >> ratio)

pTwoTimeArgsOp ::
  (Time -> Time -> Transform a) -> Parser () -> Parser (Transform a)
pTwoTimeArgsOp op name = do r1 <- name >> ratio
                            r2 <- char '\'' >> ratio
                            return $ op r1 r2
