{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Cmd2s where

import qualified Data.Set as S
import qualified Data.Map as M
import           Sound.Tidal.Epic.CombineEpics
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Types.Reimports


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

fromNonEpicCmd :: Cmd2sNonEpic -> LangNonEpic i
fromNonEpicCmd Cmd2sFast = LangNonEpicUnOp $ eFast 2
fromNonEpicCmd Cmd2sStack = LangNonEpicBinOp eStack
fromNonEpicCmd Cmd2sCat = LangNonEpicBinOp concatEpic
fromNonEpicCmd Cmd2sLeftBracket = LangNonEpicLeftBracket
fromNonEpicCmd Cmd2sRightBracket = LangNonEpicRightBracket

--cmdToLang :: forall i o. Monoidoid i o => S.Set (Cmd2s' o) -> Lang i o
--cmdToLang (Cmd2s'Epic x) = LangEpic $ cmdToAccumEpicLang x
--cmdToLang (Cmd2s'NonEpic x) = LangNonEpic $ f x

