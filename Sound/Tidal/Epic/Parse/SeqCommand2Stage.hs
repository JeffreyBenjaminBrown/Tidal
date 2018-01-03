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

cxDurScanAccum :: forall i o. Monoidoid i o =>[AccumLang o] -> [EpicOrOpIsh o]
cxDurScanAccum bs = toPartitions test f (map toEpicOrOpIsh) bs
  where test (CxCmdMap _) = True
        test _ = False
        f = map CmdMap . _cxDurScanAccum . map unwrapAccumEpicLang
  -- combined, the next two partial functions cover all AccumLang constructors
        unwrapAccumEpicLang :: AccumLang o -> AccumEpicLang o
        unwrapAccumEpicLang (CxCmdMap x) = x
        toEpicOrOpIsh :: AccumLang o -> EpicOrOpIsh o
        toEpicOrOpIsh (CxCmdUnOp x)  = CmdUnOp x
        toEpicOrOpIsh (CxCmdBinOp x) = CmdBinOp x
        toEpicOrOpIsh CxLeftBracket2s  = LeftBracket2s
        toEpicOrOpIsh CxRightBracket2s = RightBracket2s

_cxDurScanAccum :: Monoidoid i o => [AccumEpicLang o] -> [Timed o]
_cxDurScanAccum bs = map (uncurry Timed) $
  __cxDurScanAccum (1, mempty') bs

__cxDurScanAccum :: Monoidoid a t => (Dur,t) -> [AccumEpicLang t] -> [(Dur,t)]
__cxDurScanAccum priorPersistentCmds [] = []
__cxDurScanAccum (prevDur, prevMap) (AccumEpicLang mdur once persist sil : bs) =
  let next = mappend' persist prevMap
      now = foldl1 mappend' [once, persist, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall a t. Monoidoid a t => t -> t
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __cxDurScanAccum (nowDur, next) bs
