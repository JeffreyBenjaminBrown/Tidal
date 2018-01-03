{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Cmd2s where

import qualified Data.Set as S
import qualified Data.Map as M
import           Sound.Tidal.Epic.Parse.Types (
  Cmd2s(..), AccumEpicLang(..), Monoidoid(..))
import           Sound.Tidal.Epic.Types.Reimports


-- | This is a partial function. Not all Cmd2s values correspond
-- to AccumEpicLang; some are operators or brackets.
cmdToAccumEpicLang :: forall i o. Monoidoid i o =>
  S.Set (Cmd2s o) -> AccumEpicLang o
cmdToAccumEpicLang s = AccumEpicLang dur once persist silent where
  isDur, isSilent, isOnce :: Cmd2s o -> Bool
  isDur (Cmd2sDur _)   = True; isDur _    = False
  isSilent Cmd2sSilent = True; isSilent _ = False
  isOnce (Cmd2sOnce _) = True; isOnce _   = False
  (durCmds, nonDurCmds)   = S.partition isDur s
  (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
  (onceCmds, persistCmds) = S.partition isOnce paramCmds

  dur = case S.toList durCmds of (Cmd2sDur t):_ -> Just t
                                 _              -> Nothing
  silent = if S.null silentCmds then False else True

  once    = foldl1 mappend' $ cmdToPayload <$> S.toList onceCmds
  persist = foldl1 mappend' $ cmdToPayload <$> S.toList persistCmds
  cmdToPayload :: Cmd2s o -> o
  cmdToPayload Cmd2sSilent = error "cmdToPayload given silence"
  cmdToPayload (Cmd2sDur _) = error "cmdToPayload given a duration"
  cmdToPayload (Cmd2sOnce m) = m
  cmdToPayload (Cmd2sPersist m) = m
