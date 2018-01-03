module Sound.Tidal.Epic.Parse.SeqCommand
  ( Dur
  , Cmd(..)
  , CmdBlock(..)
  , blocksToEpic
  , blocksToEpic0
  , toCmdBlock
  ) where

import qualified Data.Set as S
import qualified Data.Map as M

import           Sound.Tidal.Epic.Abbreviations
import           Sound.Tidal.Epic.Params
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Parse.Types (Cmd(..),CmdBlock(..))
import qualified Sound.Tidal.Params      as P


toCmdBlock :: S.Set Cmd -> CmdBlock
toCmdBlock s = CmdBlock dur silent persist once where
  (durCmds, nonDurCmds) = S.partition isDur s
  (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
  (onceCmds, persistCmds) = S.partition isOnce paramCmds
  isDur, isSilent, isOnce :: Cmd -> Bool
  isDur (CmdDur _)        = True; isDur _    = False
  isSilent CmdSilent      = True; isSilent _ = False
  isOnce (CmdParamOnce _) = True; isOnce _   = False
  dur = case S.toList durCmds of (CmdDur t):_ -> Just t
                                 _            -> Nothing
  silent = if S.null silentCmds then False else True
  once = M.unions $ cmdToParamMap <$> S.toList onceCmds
  persist = M.unions $ cmdToParamMap <$> S.toList persistCmds
  cmdToParamMap :: Cmd -> ParamMap
  cmdToParamMap (CmdDur _) = error "cmdToParamMap given a duration"
  cmdToParamMap CmdSilent = error "cmdToParamMap given silence"
  cmdToParamMap (CmdParamOnce m) = m
  cmdToParamMap (CmdParamPersist m) = m


-- = todo ? split module: The Cmd type appears above and not below.


-- | ASSUMES: Until a duration is specified, duration defaults to 1.
-- I called this "scanAccum" because it resembles scanl and mapAccum:
-- mapAccum is to map as scanAccum is to scanl.
scanAccumBlocks :: [CmdBlock] -> [(Dur, ParamMap)]
scanAccumBlocks bs = _scanAccumBlocks (1, M.empty) bs

_scanAccumBlocks :: (Dur, ParamMap) -> [CmdBlock] -> [(Dur, ParamMap)]
_scanAccumBlocks priorPersistentCmds [] = []
_scanAccumBlocks (priorDur, priorMap) (CmdBlock mdur sil persist once : bs) =
  let nextPriorMap = M.union persist priorMap
      nowMap = M.unions [once, persist, priorMap]
      nowDur = maybe priorDur id mdur
      ignoreIfSilent :: ParamMap -> ParamMap
      ignoreIfSilent m = if sil then M.empty else m
      -- PITFALL: Uses the empty map to represent silence.
  in (nowDur, ignoreIfSilent nowMap)
     : _scanAccumBlocks (nowDur, nextPriorMap) bs

blocksToEpic0, blocksToEpic :: [CmdBlock] -> ParamEpic
blocksToEpic0 = foldl1 (+-) . map blockToEpic0 . scanAccumBlocks
blocksToEpic  = foldl1 (+-) . map blockToEpic  . scanAccumBlocks

-- PITFALL: Uses the empty map to represent silence.
blockToEpic0, blockToEpic :: (Dur, ParamMap) -> ParamEpic
blockToEpic0 (dur, map) =
  if M.null map then durSilence dur else loop0 dur map
blockToEpic  (dur, map) =
  if M.null map then durSilence dur else loopa dur map
