{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import           Sound.Tidal.Epic.Abbreviations (loopa)
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Util (toPartitions)


scanLang :: forall i o. Monoidoid i o => [Lang i o] -> [EpicOrOp i]
  -- ^ the real work is in _scanLang; the rest of this is (un)wrapping
scanLang bs = toPartitions test epicLangToEpicOrOp (map toEpicOrOp) bs
  where test (LangTerm _) = True
        test _ = False
        epicLangToEpicOrOp :: [Lang i o] -> [EpicOrOp i]
        epicLangToEpicOrOp = map (EpicNotOp . EpicWrap . timedToEpic)
                             . _scanLang . map unwrapAccumEpicLang
        timedToEpic :: Timed o -> Epic i
        timedToEpic timedo = let payload = timedPayload timedo
                                 dur = timedDur timedo
          in if null' payload then durSilence dur
             else loopa dur $ unwrap payload
  -- the next two partial funcs cover all Lang constructors
        unwrapAccumEpicLang :: Lang i o -> AccumEpicLang o
        unwrapAccumEpicLang (LangTerm x) = x
        toEpicOrOp          :: Lang i o -> EpicOrOp i
        toEpicOrOp (LangUnOp x)  = UnaryOp (UnaryWrap x)
        toEpicOrOp (LangBinOp x) = BinaryOp (BinaryWrap x)
        toEpicOrOp LangLeftBracket  = LeftBracket
        toEpicOrOp LangRightBracket = RightBracket

_scanLang :: Monoidoid i o => [AccumEpicLang o] -> [Timed o]
_scanLang bs = map (uncurry Timed) $
  __scanLang (1, mempty') bs

__scanLang :: Monoidoid i o => (Dur,o) -> [AccumEpicLang o] -> [(Dur,o)]
__scanLang priorPersistentCmds [] = []
__scanLang (prevDur, prevMap) (AccumEpicLang mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __scanLang (nowDur, next) bs
