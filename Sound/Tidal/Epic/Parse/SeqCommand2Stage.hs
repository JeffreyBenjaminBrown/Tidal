{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Util (toPartitions)


-- | ASSUMES: Until a duration is specified, duration defaults to 1.
-- I called this "scanAccum" by analogy with scanl and mapAccum:
-- mapAccum is to map as scanAccum is to scanl.


cxDurScanAccum :: forall i o. Monoidoid i o =>
  [AccumLang i o] -> [EpicOrOpIsh i o]
cxDurScanAccum bs = toPartitions test f (map toEpicOrOpIsh) bs
  where test (AccumLangTerm _) = True
        test _ = False
        f = map CmdTerm . _cxDurScanAccum . map unwrapAccumEpicLang
  -- combined, the next two partial functions cover all AccumLang constructors
        unwrapAccumEpicLang :: AccumLang i o -> AccumEpicLang o
        unwrapAccumEpicLang (AccumLangTerm x) = x
        toEpicOrOpIsh :: AccumLang i o -> EpicOrOpIsh i o
        toEpicOrOpIsh (AccumLangUnOp x)  = CmdUnOp x
        toEpicOrOpIsh (AccumLangBinOp x) = CmdBinOp x
        toEpicOrOpIsh AccumLangLeftBracket  = CmdLeftBracket
        toEpicOrOpIsh AccumLangRightBracket = CmdRightBracket

_cxDurScanAccum :: Monoidoid i o => [AccumEpicLang o] -> [Timed o]
_cxDurScanAccum bs = map (uncurry Timed) $
  __cxDurScanAccum (1, mempty') bs

__cxDurScanAccum :: Monoidoid i o => (Dur,o) -> [AccumEpicLang o] -> [(Dur,o)]
__cxDurScanAccum priorPersistentCmds [] = []
__cxDurScanAccum (prevDur, prevMap) (AccumEpicLang mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __cxDurScanAccum (nowDur, next) bs
