{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Lexeme where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Ratio
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)

import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Parse.Types

import           Sound.Tidal.Epic.CombineEpics
import           Sound.Tidal.Epic.Parse.Eq
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Parse.Expr (parseEpicExpr)
import qualified Sound.Tidal.Epic.Parse.Phoneme.Map      as PM
import qualified Sound.Tidal.Epic.Parse.Phoneme.ParamMap as PPM
import qualified Sound.Tidal.Epic.Parse.Phoneme.Scale    as Sc
import qualified Sound.Tidal.Epic.Parse.Phoneme.Number   as Number
import           Sound.Tidal.Epic.Parse.Transform (scanLang, lexemeToAccumEpic)
import           Sound.Tidal.Epic.Parse.Util


doubleParse :: (a -> Epic b) -> (a -> Epic b) -> (a -> (Epic b, Epic b))
doubleParse f g s = (f s, g s)

_p :: ((Time -> i -> Epic i) -> Parser [EpicOrOp i])
   -> (Time -> i -> Epic i) -> String -> Epic i
_p p loopx s = case parse (sc >> p loopx) "" s of
  Left e -> error $ show e
  Right r -> case parse parseEpicExpr "" r of
    Left e -> error "unshowable Epic ParamMap parse error"
    Right r -> r
pe,pe0 :: String -> Epic ParamMap
pe  = _p peEpicOrOps loopa
pe0 = _p peEpicOrOps loop0
pe2 = doubleParse pe pe0

pm,pm0 :: String -> Epic PM.MSD
pm  = _p pmEpicOrOps loopa
pm0 = _p pmEpicOrOps loop0
pm2 = doubleParse pm pm0

ps :: String -> Epic Scale
ps  = _p psEpicOrOps loopa
ps0 = _p psEpicOrOps loop0
ps2 = doubleParse ps ps0

pd :: String -> Epic Double
pd  = _p pdEpicOrOps loopa
pd0 = _p pdEpicOrOps loop0
pd2 = doubleParse pd pd0

pr :: String -> Epic (Ratio Integer)
pr  = _p prEpicOrOps loopa
pr0 = _p prEpicOrOps loop0
pr2 = doubleParse pr pr0

pEpicOrOps :: (Monoidoid i o, Ord o) =>
  Parser [Lang i o] -> (Time -> i -> Epic i) -> Parser [EpicOrOp i]
pEpicOrOps p loopx = scanLang loopx <$> p
peEpicOrOps ::
  (Time -> ParamMap -> Epic ParamMap) -> Parser [EpicOrOp ParamMap]
peEpicOrOps = pEpicOrOps peLang
pmEpicOrOps ::
  (Time -> PM.MSD -> Epic PM.MSD) -> Parser [EpicOrOp PM.MSD]
pmEpicOrOps = pEpicOrOps pmLang
psEpicOrOps :: (Time -> Scale -> Epic Scale) -> Parser [EpicOrOp Scale]
psEpicOrOps = pEpicOrOps psLang
pdEpicOrOps :: (Time -> Double -> Epic Double) -> Parser [EpicOrOp Double]
pdEpicOrOps = pEpicOrOps pdLang
prEpicOrOps :: Integral a =>
  (Time -> (Ratio a) -> Epic (Ratio a)) -> Parser [EpicOrOp (Ratio a)]
prEpicOrOps = pEpicOrOps prLang

pLang :: (Monoidoid i o, Ord o) => Parser [Lexeme i o] -> Parser [Lang i o]
pLang p = map f <$> p where
  f c = case c of
    LexemeEpics list -> LangEpic $ lexemeToAccumEpic $ S.fromList list
    LexemeNonEpic nonEpic -> LangNonEpic nonEpic
peLang :: Parser [Lang ParamMap ParamMap]
peLang = pLang peLexemes
pmLang :: Parser [Lang PM.MSD PM.MSD]
pmLang = pLang pmLexemes
psLang :: Parser [Lang Scale (Maybe Scale)]
psLang = pLang psLexemes
pdLang :: Parser [Lang Double (Maybe Double)]
pdLang = pLang pdLexemes
prLang :: Integral a => Parser [Lang (Ratio a) (Maybe (Ratio a))]
prLang = pLang prLexemes

pLexemes :: Monoidoid i o => Parser (Lexeme i o) -> Parser [Lexeme i o]
pLexemes p = some $ try p <|> try pLexemeNonEpicLexeme
peLexemes :: Parser [Lexeme ParamMap ParamMap]
peLexemes = pLexemes peLexemeEpics
pmLexemes :: Parser [Lexeme PM.MSD PM.MSD]
pmLexemes = pLexemes pmLexemeEpics
psLexemes :: Parser [Lexeme Scale (Maybe Scale)]
psLexemes = pLexemes psLexemeEpics
pdLexemes :: Parser [Lexeme Double (Maybe Double)]
pdLexemes = pLexemes pdLexemeEpics
prLexemes :: Integral a => Parser [Lexeme (Ratio a) (Maybe (Ratio a))]
prLexemes = pLexemes prLexemeEpics

pLexemeEpics :: Monoidoid i o => (Parser (EpicPhoneme o)) -> Parser (Lexeme i o)
pLexemeEpics p = lexeme $ LexemeEpics <$> sepBy1 p (some $ char ',')
peLexemeEpics :: Parser (Lexeme ParamMap ParamMap)
peLexemeEpics = pLexemeEpics PPM.epicPhoneme
pmLexemeEpics :: Parser (Lexeme PM.MSD PM.MSD)
pmLexemeEpics = pLexemeEpics PM.epicPhoneme
psLexemeEpics :: Parser (Lexeme Scale (Maybe Scale))
psLexemeEpics = pLexemeEpics Sc.epicPhoneme
pdLexemeEpics :: Parser (Lexeme Double (Maybe Double))
pdLexemeEpics = pLexemeEpics Number.epicPhonemeDouble
prLexemeEpics :: Integral a => Parser (Lexeme (Ratio a) (Maybe (Ratio a)))
prLexemeEpics = pLexemeEpics Number.epicPhonemeRatio


-- | = The code below does not depend on the payload (ParamMap, scale, etc.)
pLexemeNonEpicLexeme :: Parser (Lexeme i o)
pLexemeNonEpicLexeme = LexemeNonEpic <$> pNonEpicLexeme

pNonEpicLexeme :: Parser (NonEpicLexeme i)
pNonEpicLexeme = pUnOp <|> pBinOp <|> pBracket

pUnOp :: Parser (NonEpicLexeme i)
pUnOp = NonEpicLexemeUnOp . foldl1 (.) <$> some pSingleUnOp

pSingleUnOp :: Parser (Epic a -> Epic a)
pSingleUnOp = foldl1 (<|>) $ map try
  [pNonEpicLexemeFast, pNonEpicLexemeSlow, pNonEpicLexemeDense, pNonEpicLexemeSparse, pNonEpicLexemeRotate, pNonEpicLexemeEarly, pNonEpicLexemeLate]

pNonEpicLexemeFast, pNonEpicLexemeSlow, pNonEpicLexemeDense, pNonEpicLexemeSparse, pNonEpicLexemeRotate, pNonEpicLexemeEarly, pNonEpicLexemeLate :: Parser (Epic a -> Epic a)
pNonEpicLexemeFast = lexeme $ do n <- symbol "*" >> ratio
                                 return $ fast n
pNonEpicLexemeSlow = lexeme $ do n <- symbol "/" >> ratio
                                 return $ slow n
pNonEpicLexemeDense = lexeme $ do n <- symbol "**" >> ratio
                                  return $ dense n
pNonEpicLexemeSparse = lexeme $ do n <- symbol "//" >> ratio
                                   return $ sparse n
pNonEpicLexemeRotate = lexeme $ do n <- symbol "*//" >> ratio
                                   return $ fast n . sparse n
pNonEpicLexemeEarly = lexeme $ do n <- symbol "<" >> ratio
                                  return $ early n
pNonEpicLexemeLate = lexeme $ do n <- symbol ">" >> ratio
                                 return $ late n

pBinOp, pNonEpicLexemstack, pNonEpicLexemeCat :: Parser (NonEpicLexeme i)
pBinOp = try pNonEpicLexemstack <|> try pNonEpicLexemeCat
pNonEpicLexemstack = do lexeme $ symbol "|"
                        return $ NonEpicLexemeBinOp stack
pNonEpicLexemeCat = do lexeme $ symbol "-"
                       return $ NonEpicLexemeBinOp append

pBracket, pNonEpicLexemeLeftBracket, pNonEpicLexemeRightBracket :: Parser (NonEpicLexeme i)
pBracket = pNonEpicLexemeLeftBracket <|> pNonEpicLexemeRightBracket
pNonEpicLexemeLeftBracket = do lexeme $ symbol "["
                               return NonEpicLexemeLeftBracket
pNonEpicLexemeRightBracket = do lexeme $ symbol "]"
                                return NonEpicLexemeRightBracket
