{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import           Sound.Tidal.Epic.Abbreviations (loopa)
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Util (toPartitions)


scanAccumLang :: forall i o. Monoidoid i o => [AccumLang i o] -> [EpicOrOp i]
  -- ^ the real work is in _scanAccumLang; the rest of this is (un)wrapping
scanAccumLang bs = toPartitions test epicLangToEpicOrOp (map toEpicOrOp) bs
  where test (AccumLangTerm _) = True
        test _ = False
        epicLangToEpicOrOp :: [AccumLang i o] -> [EpicOrOp i]
        epicLangToEpicOrOp = map (EpicNotOp . EpicWrap . timedToEpic)
                             . _scanAccumLang . map unwrapAccumEpicLang
        timedToEpic :: Timed o -> Epic i
        timedToEpic timedo = let payload = timedPayload timedo
                                 dur = timedDur timedo
          in if null' payload then durSilence dur
             else loopa dur $ unwrap payload
  -- the next two partial funcs cover all AccumLang constructors
        unwrapAccumEpicLang :: AccumLang i o -> AccumEpicLang o
        unwrapAccumEpicLang (AccumLangTerm x) = x
        toEpicOrOp          :: AccumLang i o -> EpicOrOp i
        toEpicOrOp (AccumLangUnOp x)  = UnaryOp (UnaryWrap x)
        toEpicOrOp (AccumLangBinOp x) = BinaryOp (BinaryWrap x)
        toEpicOrOp AccumLangLeftBracket  = LeftBracket
        toEpicOrOp AccumLangRightBracket = RightBracket

_scanAccumLang :: Monoidoid i o => [AccumEpicLang o] -> [Timed o]
_scanAccumLang bs = map (uncurry Timed) $
  __scanAccumLang (1, mempty') bs

__scanAccumLang :: Monoidoid i o => (Dur,o) -> [AccumEpicLang o] -> [(Dur,o)]
__scanAccumLang priorPersistentCmds [] = []
__scanAccumLang (prevDur, prevMap) (AccumEpicLang mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __scanAccumLang (nowDur, next) bs
