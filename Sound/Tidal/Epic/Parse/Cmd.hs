{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Cmd where

import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)

import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Parse.Types

import           Sound.Tidal.Epic.Abbreviations (loopa, loop0)
import           Sound.Tidal.Epic.CombineEpics
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Parse.EpicOrOp (parseEpicExpr)
import           Sound.Tidal.Epic.Parse.SingletonMap (pSingleton)
import           Sound.Tidal.Epic.Parse.SeqCommand (scanLang)
import           Sound.Tidal.Epic.Parse.Util


_pEpic :: (Time -> ParamMap -> Epic ParamMap) -> String -> Epic ParamMap
_pEpic loopx s = case parse (pEpicOrOps loopx) "" s of
  Left e -> error $ show e
  Right r -> case parse parseEpicExpr "" r of
    Left e -> error "unshowable Epic ParamMap parse error"
    Right r -> r
pEpic, pEpic0 :: String -> Epic ParamMap
pEpic = _pEpic loopa
pEpic0 = _pEpic loop0

pEpicOrOps :: (Time -> ParamMap -> Epic ParamMap)
            -> Parser [EpicOrOp ParamMap]
pEpicOrOps loopx = scanLang loopx <$> pLang

pLang :: Parser [Lang ParamMap ParamMap]
pLang = map f <$> pCmds where
  f c = case c of
    CmdEpics list -> LangEpic $ cmdToAccumEpic $ S.fromList list
    CmdNonEpic nonEpic -> LangNonEpic nonEpic

cmdToAccumEpic :: forall i o. Monoidoid i o =>
  S.Set (EpicLexeme o) -> AccumEpic o
cmdToAccumEpic s = AccumEpic dur once persist silent where
  isDur, isSilent, isOnce :: EpicLexeme o -> Bool
  isDur (EpicLexemeDur _)   = True; isDur _    = False
  isSilent EpicLexemeSilent = True; isSilent _ = False
  isOnce (EpicLexemeOnce _) = True; isOnce _   = False
  (durCmds, nonDurCmds)   = S.partition isDur s
  (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
  (onceCmds, persistCmds) = S.partition isOnce paramCmds

  dur = case S.toList durCmds of (EpicLexemeDur t):_ -> Just t
                                 _               -> Nothing
  silent = if S.null silentCmds then False else True
  once    = foldl mappend' mempty' $ cmdToPayload <$> S.toList onceCmds
  persist = foldl mappend' mempty' $ cmdToPayload <$> S.toList persistCmds

  cmdToPayload :: EpicLexeme o -> o
  cmdToPayload  EpicLexemeSilent = error "cmdToPayload given silence"
  cmdToPayload (EpicLexemeDur _) = error "cmdToPayload given a duration"
  cmdToPayload (EpicLexemeOnce m) = m
  cmdToPayload (EpicLexemeNewPersist m) = m

pCmds :: Parser [Cmd ParamMap ParamMap]
pCmds = concat <$> some f where f = pCmdEpics
                                      <|> (:[]) <$> pCmdCmdNonEpic
pCmdEpics :: Parser [Cmd ParamMap ParamMap]
pCmdEpics = sepBy1 (CmdEpics <$> some epicLexeme) (lexeme $ string ",,")
  -- TODO ? ',,' cannot yet be used like other binary operators;
  -- if one of its arguments is bracketed, it fails
pCmdCmdNonEpic :: Parser (Cmd ParamMap ParamMap)
pCmdCmdNonEpic = CmdNonEpic <$> pLangNonEpic


epicLexeme, epicLexemePersist, epicLexemeOnce, epicLexemeDur ::
  Parser (EpicLexeme ParamMap)
epicLexeme = foldl1 (<|>) [epicLexemePersist, epicLexemeOnce, epicLexemeDur, cmdSilence]
epicLexemePersist = lexeme $ EpicLexemeNewPersist <$> pSingleton
epicLexemeOnce = lexeme $ EpicLexemeOnce <$> (ignore (char '1') >> pSingleton)
epicLexemeDur = lexeme $ ignore (char 't') >> EpicLexemeDur <$> ratio
  -- >> TODO: accept floats as well as ratios
cmdSilence = lexeme $ const EpicLexemeSilent <$> char '_'


pLangNonEpic, pLangNonEpicFast, pLangNonEpicStack, pLangNonEpicCat,
  pLangNonEpicLeftBracket, pLangNonEpicRightBracket :: Parser (LangNonEpic i)
pLangNonEpic = foldr1 (<|>) $ map try $ [ pLangNonEpicFast
                                        , pLangNonEpicStack
                                        , pLangNonEpicCat
                                        , pLangNonEpicLeftBracket
                                        , pLangNonEpicRightBracket
                                        ]
pLangNonEpicFast = do lexeme $ symbol "fast"
                      return $ LangNonEpicUnOp $ eFast 2
pLangNonEpicStack = do lexeme $ symbol "stack"
                       return $ LangNonEpicBinOp eStack
pLangNonEpicCat = do lexeme $ symbol "cat"
                     return $ LangNonEpicBinOp concatEpic
pLangNonEpicLeftBracket = do lexeme $ symbol "["
                             return LangNonEpicLeftBracket
pLangNonEpicRightBracket = do lexeme $ symbol "]"
                              return LangNonEpicRightBracket
