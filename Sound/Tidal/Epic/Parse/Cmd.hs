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
    CmdEpics list -> LangEpic $ cmdToAccumEpicLang $ S.fromList list
    CmdNonEpic langNonEpic -> LangNonEpic langNonEpic

cmdToAccumEpicLang :: forall i o. Monoidoid i o =>
  S.Set (CmdEpic o) -> AccumEpicLang o
cmdToAccumEpicLang s = AccumEpicLang dur once persist silent where
  isDur, isSilent, isOnce :: CmdEpic o -> Bool
  isDur (CmdEpicDur _)   = True; isDur _    = False
  isSilent CmdEpicSilent = True; isSilent _ = False
  isOnce (CmdEpicOnce _) = True; isOnce _   = False
  (durCmds, nonDurCmds)   = S.partition isDur s
  (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
  (onceCmds, persistCmds) = S.partition isOnce paramCmds

  dur = case S.toList durCmds of (CmdEpicDur t):_ -> Just t
                                 _               -> Nothing
  silent = if S.null silentCmds then False else True
  once    = foldl mappend' mempty' $ cmdToPayload <$> S.toList onceCmds
  persist = foldl mappend' mempty' $ cmdToPayload <$> S.toList persistCmds

  cmdToPayload :: CmdEpic o -> o
  cmdToPayload  CmdEpicSilent = error "cmdToPayload given silence"
  cmdToPayload (CmdEpicDur _) = error "cmdToPayload given a duration"
  cmdToPayload (CmdEpicOnce m) = m
  cmdToPayload (CmdEpicNewPersist m) = m

pCmds :: Parser [Cmd ParamMap ParamMap]
pCmds = concat <$> some f where f = pCmdCmdEpics
                                      <|> (:[]) <$> pCmdCmdNonEpic
pCmdCmdEpics :: Parser [Cmd ParamMap ParamMap]
pCmdCmdEpics = sepBy1 (CmdEpics <$> some cmdEpic) (lexeme $ string ",,")
  -- TODO ? ',,' cannot yet be used like other binary operators;
  -- if one of its arguments is bracketed, it fails
pCmdCmdNonEpic :: Parser (Cmd ParamMap ParamMap)
pCmdCmdNonEpic = CmdNonEpic <$> pLangNonEpic


cmdEpic, cmdPersist, cmdOnce, cmdDur ::
  Parser (CmdEpic ParamMap)
cmdEpic = foldl1 (<|>) [cmdPersist, cmdOnce, cmdDur, cmdSilence]
cmdPersist = lexeme $ CmdEpicNewPersist <$> pSingleton
cmdOnce = lexeme $ CmdEpicOnce <$> (ignore (char '1') >> pSingleton)
cmdDur = lexeme $ ignore (char 't') >> CmdEpicDur <$> ratio
  -- >> TODO: accept floats as well as ratios
cmdSilence = lexeme $ const CmdEpicSilent <$> char '_'


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
