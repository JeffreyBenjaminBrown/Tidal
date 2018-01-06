{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Cmd2s where

import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)

import           Sound.Tidal.Epic.CombineEpics
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Parse.Cmd (parseSingleton)
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Parse.Util


cmdToAccumEpicLang :: forall i o. Monoidoid i o =>
  S.Set (Cmd2sEpic o) -> AccumEpicLang o
cmdToAccumEpicLang s = AccumEpicLang dur once persist silent where
  isDur, isSilent, isOnce :: Cmd2sEpic o -> Bool
  isDur (Cmd2sEpicDur _)   = True; isDur _    = False
  isSilent Cmd2sEpicSilent = True; isSilent _ = False
  isOnce (Cmd2sEpicOnce _) = True; isOnce _   = False
  (durCmds, nonDurCmds)   = S.partition isDur s
  (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
  (onceCmds, persistCmds) = S.partition isOnce paramCmds

  dur = case S.toList durCmds of (Cmd2sEpicDur t):_ -> Just t
                                 _               -> Nothing
  silent = if S.null silentCmds then False else True
  once    = foldl1 mappend' $ cmdToPayload <$> S.toList onceCmds
  persist = foldl1 mappend' $ cmdToPayload <$> S.toList persistCmds

  cmdToPayload :: Cmd2sEpic o -> o
  cmdToPayload  Cmd2sEpicSilent = error "cmdToPayload given silence"
  cmdToPayload (Cmd2sEpicDur _) = error "cmdToPayload given a duration"
  cmdToPayload (Cmd2sEpicOnce m) = m
  cmdToPayload (Cmd2sEpicPersist m) = m


pCmd2s, pCmd2sCmdEpic, pCmd2sCmdNonEpic :: Parser (Cmd2s ParamMap ParamMap)
pCmd2s = pCmd2sCmdEpic <|> pCmd2sCmdNonEpic
pCmd2sCmdEpic = Cmd2sEpic <$> cmd2sEpic
pCmd2sCmdNonEpic = Cmd2sNonEpic <$> pLangNonEpic


cmd2sEpic, cmd2sPersist, cmd2sOnce, cmd2sDur ::
  Parser (Cmd2sEpic ParamMap)
cmd2sEpic = foldl1 (<|>) [cmd2sPersist, cmd2sOnce, cmd2sDur, cmd2sSilence]
cmd2sPersist = lexeme $ Cmd2sEpicPersist <$> parseSingleton
cmd2sOnce = lexeme $ Cmd2sEpicOnce <$> (ignore (char '1') >> parseSingleton)
cmd2sDur = lexeme $ ignore (char 't') >> Cmd2sEpicDur <$> ratio
cmd2sSilence = lexeme $ const Cmd2sEpicSilent <$> char '_'


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
