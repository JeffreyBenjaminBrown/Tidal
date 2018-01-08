{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types

import           Sound.Tidal.Epic.Abbreviations (loopa, loop0)
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Util (toPartitions)


-- | todo ? ask ? ugly: timedToEpic and timedToEpic0 differ by one character
-- A tree of calling functions bifurcates from this distinction.
_timedToEpic :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> Timed o -> Epic i
_timedToEpic loopx timedo = let payload = timedPayload timedo
                                dur = timedDur timedo
                            in if null' payload then durSilence dur
                               else loopx dur $ unwrap payload
timedToEpic, timedToEpic0 :: Monoidoid i o => Timed o -> Epic i
timedToEpic = _timedToEpic loopa
timedToEpic0 = _timedToEpic loop0

_fromAccumEpicLang :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> [AccumEpicLang o] -> [EpicOrOp i]
_fromAccumEpicLang loopx = map (EpicNotOp . EpicWrap . _timedToEpic loopx)
                           . _scanAccumEpicLang
fromAccumEpicLang, fromAccumEpicLang0 :: Monoidoid i o =>
  [AccumEpicLang o] -> [EpicOrOp i]
fromAccumEpicLang = _fromAccumEpicLang loopa
fromAccumEpicLang0 = _fromAccumEpicLang loop0

fromLangNonEpic :: LangNonEpic i -> EpicOrOp i
fromLangNonEpic (LangNonEpicUnOp x)  = UnaryOp (UnaryWrap x)
fromLangNonEpic (LangNonEpicBinOp x) = BinaryOp (BinaryWrap x)
fromLangNonEpic LangNonEpicLeftBracket  = LeftBracket
fromLangNonEpic LangNonEpicRightBracket = RightBracket

_scanLang :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> [Lang i o] -> [EpicOrOp i]
_scanLang loopx bs = toPartitions test
                            (_fromAccumEpicLang loopx . map unwrapEpic)
                            (map $ fromLangNonEpic . unwrapNonEpic)
                            bs
  where test (LangEpic _) = True
        test (LangNonEpic _) = False
        unwrapEpic (LangEpic x) = x
        unwrapNonEpic (LangNonEpic x) = x
scanLang, scanLang0 :: forall i o. Monoidoid i o => [Lang i o] -> [EpicOrOp i]
scanLang = _scanLang loopa
scanLang0 = _scanLang loop0

_scanAccumEpicLang :: Monoidoid i o => [AccumEpicLang o] -> [Timed o]
_scanAccumEpicLang bs = map (uncurry Timed) $
  __scanAccumEpicLang (1, mempty') bs

__scanAccumEpicLang :: Monoidoid i o => (Dur,o) -> [AccumEpicLang o] -> [(Dur,o)]
__scanAccumEpicLang priorPersistentCmds [] = []
__scanAccumEpicLang (prevDur, prevMap) (AccumEpicLang mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __scanAccumEpicLang (nowDur, next) bs
