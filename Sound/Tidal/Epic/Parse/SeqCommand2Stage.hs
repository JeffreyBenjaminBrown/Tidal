{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import           Sound.Tidal.Epic.Abbreviations (loopa)
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Util (toPartitions)


timedToEpic :: Monoidoid i o => Timed o -> Epic i
timedToEpic timedo = let payload = timedPayload timedo
                         dur = timedDur timedo
                     in if null' payload then durSilence dur
                        else loopa dur $ unwrap payload

fromAccumEpicLang :: Monoidoid i o => [AccumEpicLang o] -> [EpicOrOp i]
fromAccumEpicLang = map (EpicNotOp . EpicWrap . timedToEpic)
                    . _scanAccumEpicLang

fromLangNonEpic :: LangNonEpic i -> EpicOrOp i
fromLangNonEpic (LangNonEpicUnOp x)  = UnaryOp (UnaryWrap x)
fromLangNonEpic (LangNonEpicBinOp x) = BinaryOp (BinaryWrap x)
fromLangNonEpic LangNonEpicLeftBracket  = LeftBracket
fromLangNonEpic LangNonEpicRightBracket = RightBracket

scanLang :: forall i o. Monoidoid i o => [Lang i o] -> [EpicOrOp i]
scanLang bs = toPartitions test
                            (fromAccumEpicLang . map unwrapEpic)
                            (map $ fromLangNonEpic . unwrapNonEpic)
                            bs
  where test (LangEpic _) = True
        test (LangNonEpic _) = False
        unwrapEpic (LangEpic x) = x
        unwrapNonEpic (LangNonEpic x) = x

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
