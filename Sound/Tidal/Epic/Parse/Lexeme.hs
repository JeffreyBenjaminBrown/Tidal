{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Lexeme where

import qualified Data.Set as S
import qualified Data.Map as M
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
import qualified Sound.Tidal.Epic.Parse.ParamMap as PM
import qualified Sound.Tidal.Epic.Parse.Scale    as Sc
import           Sound.Tidal.Epic.Parse.Transform (scanLang, lexemeToAccumEpic)
import           Sound.Tidal.Epic.Parse.Util


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
ps  = _p psEpicOrOps loopa
ps0 = _p psEpicOrOps loop0

pEpicOrOps :: (Monoidoid i o, Ord o) =>
  Parser [Lang i o] -> (Time -> i -> Epic i) -> Parser [EpicOrOp i]
pEpicOrOps p loopx = scanLang loopx <$> p
peEpicOrOps ::
  (Time -> ParamMap -> Epic ParamMap) -> Parser [EpicOrOp ParamMap]
peEpicOrOps = pEpicOrOps peLang
psEpicOrOps :: (Time -> Scale -> Epic Scale) -> Parser [EpicOrOp Scale]
psEpicOrOps = pEpicOrOps psLang

pLang :: (Monoidoid i o, Ord o) => Parser [Lexeme i o] -> Parser [Lang i o]
pLang p = map f <$> p where
  f c = case c of
    LexemeEpics list -> LangEpic $ lexemeToAccumEpic $ S.fromList list
    LexemeNonEpic nonEpic -> LangNonEpic nonEpic
peLang :: Parser [Lang ParamMap ParamMap]
peLang = pLang peLexemes
psLang :: Parser [Lang Scale (Maybe Scale)]
psLang = pLang psLexemes

pLexemes :: Monoidoid i o => Parser (Lexeme i o) -> Parser [Lexeme i o]
pLexemes p = some $ p <|> pLexemeNonEpicLexeme
peLexemes :: Parser [Lexeme ParamMap ParamMap]
peLexemes = pLexemes peLexemeEpics
psLexemes :: Parser [Lexeme Scale (Maybe Scale)]
psLexemes = pLexemes psLexemeEpics

pLexemeEpics :: Monoidoid i o => (Parser (EpicLexeme o)) -> Parser (Lexeme i o)
pLexemeEpics p = LexemeEpics <$> some p
peLexemeEpics :: Parser (Lexeme ParamMap ParamMap)
peLexemeEpics = pLexemeEpics PM.epicLexeme
psLexemeEpics :: Parser (Lexeme Scale (Maybe Scale))
psLexemeEpics = pLexemeEpics Sc.epicLexeme


-- | = The code below does not depend on the payload (ParamMap, scale, etc.)
pLexemeNonEpicLexeme :: Parser (Lexeme i o)
pLexemeNonEpicLexeme = LexemeNonEpic <$> pNonEpicLexeme

pNonEpicLexeme :: Parser (NonEpicLexeme i)
pNonEpicLexeme = pUnOp <|> pBinOp <|> pBracket

pUnOp :: Parser (NonEpicLexeme i)
pUnOp = NonEpicLexemeUnOp . foldl1 (.) <$> some pSingleUnOp

pSingleUnOp :: Parser (Epic a -> Epic a)
pSingleUnOp = foldl1 (<|>) $ map try
  [pNonEpicLexemeFast, pNonEpicLexemeSlow, pNonEpicLexemeDense, pNonEpicLexemeSparse, pNonEpicLexemeEarly, pNonEpicLexemeLate]

pNonEpicLexemeFast, pNonEpicLexemeSlow, pNonEpicLexemeDense, pNonEpicLexemeSparse, pNonEpicLexemeEarly, pNonEpicLexemeLate :: Parser (Epic a -> Epic a)
pNonEpicLexemeFast = lexeme $ do n <- symbol "*" >> ratio
                                 return $ eFast n
pNonEpicLexemeSlow = lexeme $ do n <- symbol "/" >> ratio
                                 return $ eSlow n
pNonEpicLexemeDense = lexeme $ do n <- symbol "**" >> ratio
                                  return $dense n
pNonEpicLexemeSparse = lexeme $ do n <- symbol "//" >> ratio
                                   return $ sparse n
pNonEpicLexemeEarly = lexeme $ do n <- symbol "<" >> ratio
                                  return $ early n
pNonEpicLexemeLate = lexeme $ do n <- symbol ">" >> ratio
                                 return $ late n

pBinOp, pNonEpicLexemeStack, pNonEpicLexemeCat :: Parser (NonEpicLexeme i)
pBinOp = try pNonEpicLexemeStack <|> try pNonEpicLexemeCat
pNonEpicLexemeStack = do lexeme $ symbol "+|"
                         return $ NonEpicLexemeBinOp eStack
pNonEpicLexemeCat = do lexeme $ symbol "+-"
                       return $ NonEpicLexemeBinOp concatEpic

pBracket, pNonEpicLexemeLeftBracket, pNonEpicLexemeRightBracket :: Parser (NonEpicLexeme i)
pBracket = pNonEpicLexemeLeftBracket <|> pNonEpicLexemeRightBracket
pNonEpicLexemeLeftBracket = do lexeme $ symbol "["
                               return NonEpicLexemeLeftBracket
pNonEpicLexemeRightBracket = do lexeme $ symbol "]"
                                return NonEpicLexemeRightBracket
