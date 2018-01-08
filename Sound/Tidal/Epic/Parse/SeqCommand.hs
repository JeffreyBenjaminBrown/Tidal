{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand where

import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types

import           Sound.Tidal.Epic.Abbreviations (loopa, loop0)
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Util (toPartitions)


timedToEpic :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> Timed o -> Epic i
timedToEpic loopx timedo = let payload = timedPayload timedo
                               dur = timedDur timedo
                           in if null' payload then durSilence dur
                              else loopx dur $ unwrap payload

fromAccumEpic :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> [AccumEpic o] -> [EpicOrOp i]
fromAccumEpic loopx = map (EpicNotOp . EpicWrap . timedToEpic loopx)
                           . _scanAccumEpic

fromLangNonEpic :: LangNonEpic i -> EpicOrOp i
fromLangNonEpic (LangNonEpicUnOp x)  = UnaryOp (UnaryWrap x)
fromLangNonEpic (LangNonEpicBinOp x) = BinaryOp (BinaryWrap x)
fromLangNonEpic LangNonEpicLeftBracket  = LeftBracket
fromLangNonEpic LangNonEpicRightBracket = RightBracket

scanLang :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> [Lang i o] -> [EpicOrOp i]
scanLang loopx bs = toPartitions test
                            (fromAccumEpic loopx . map unwrapEpic)
                            (map $ fromLangNonEpic . unwrapNonEpic)
                            bs
  where test (LangEpic _) = True
        test (LangNonEpic _) = False
        unwrapEpic (LangEpic x) = x
        unwrapNonEpic (LangNonEpic x) = x

_scanAccumEpic :: Monoidoid i o => [AccumEpic o] -> [Timed o]
_scanAccumEpic bs = map (uncurry Timed) $
  __scanAccumEpic (1, mempty') bs

__scanAccumEpic :: Monoidoid i o => (Dur,o) -> [AccumEpic o] -> [(Dur,o)]
__scanAccumEpic priorPersistentCmds [] = []
__scanAccumEpic (prevDur, prevMap) (AccumEpic mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __scanAccumEpic (nowDur, next) bs
