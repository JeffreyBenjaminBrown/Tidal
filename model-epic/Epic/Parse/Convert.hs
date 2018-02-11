-- | = Among the types in Parse.Types, the two tricky transformations in
-- Parse.Transform involve two different ways of accumulating
-- the information in Lexemes.
--
-- The first to run, `lexemeToAccumEpic`, accumulates a set of instructions
-- like "t1%2 d1" into an AccumEpic (in this case, one with a duration of 1/2
-- carrying a persistent singleton map from deg_p to VF 1) and
-- no temporary map.
--
-- After that, `scanLang` scans across a series of AccumEpics to
-- creates EpicOrOp values, where each contains the maps not just of the
-- Lexeme corresponding to it, but also the persistent maps in all the prior
-- Lexemes. (It might not be a map, though; it just has to be a Monoidoid.)

{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Convert where

import qualified Data.Set as S
import qualified Data.List as L
import           Sound.Tidal.Epic.Parse.Types
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types

import           Sound.Tidal.Epic.Transform (durSilence, loopa, loop0)
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

fromNonEpicLexeme :: NonEpicLexeme i -> EpicOrOp i
fromNonEpicLexeme (NonEpicLexemeUnOp x)  = UnaryOp (UnaryWrap x)
fromNonEpicLexeme (NonEpicLexemeBinOp x) = BinaryOp (BinaryWrap x)
fromNonEpicLexeme NonEpicLexemeLeftBracket  = LeftBracket
fromNonEpicLexeme NonEpicLexemeRightBracket = RightBracket

scanLang :: forall i o. Monoidoid i o =>
  (Time -> i -> Epic i) -> [Lang i o] -> [EpicOrOp i]
scanLang loopx bs = toPartitions test
                            (fromAccumEpic loopx . map unwrapEpic)
                            (map $ fromNonEpicLexeme . unwrapNonEpic)
                            bs
  where test (LangEpic _) = True
        test (LangNonEpic _) = False
        unwrapEpic (LangEpic x) = x
        unwrapNonEpic (LangNonEpic x) = x

_scanAccumEpic :: Monoidoid i o => [AccumEpic o] -> [Timed o]
_scanAccumEpic bs = map (uncurry Timed) $
  __scanAccumEpic (1, mempty') bs

_scanAccumEpicTimeless :: Monoidoid i o => [AccumEpic o] -> [o]
_scanAccumEpicTimeless bs = map snd $ __scanAccumEpic (1, mempty') bs

__scanAccumEpic :: Monoidoid i o => (Dur,o) -> [AccumEpic o] -> [(Dur,o)]
__scanAccumEpic priorPersistentLexemes [] = []
__scanAccumEpic (prevDur, prevMap) (AccumEpic mdur temp keep sil : bs) =
  let next = mappend' keep prevMap
      now = foldl1 mappend' [temp, keep, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: forall i o. Monoidoid i o => o -> o
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : __scanAccumEpic (nowDur, next) bs

lexemeToAccumEpic :: forall i o. Monoidoid i o =>
  [EpicPhoneme o] -> AccumEpic o
lexemeToAccumEpic s = AccumEpic dur once persist silent where
  isDur, isSilent, isOnce :: EpicPhoneme o -> Bool
  isDur (EpicPhonemeFor _)   = True; isDur _    = False
  isSilent EpicPhonemeSilent = True; isSilent _ = False
  isOnce (EpicPhonemeOnce _) = True; isOnce _   = False
  (durLexemes, nonDurLexemes)   = L.partition isDur s
  (silentLexemes, paramLexemes) = L.partition isSilent nonDurLexemes
  (onceLexemes, persistLexemes) = L.partition isOnce paramLexemes

  dur = case durLexemes of (EpicPhonemeFor t):_ -> Just t
                           _                    -> Nothing
  silent = if L.null silentLexemes then False else True
  once    = foldl mappend' mempty' $ lexemeToPayload <$> onceLexemes
  persist = foldl mappend' mempty' $ lexemeToPayload <$> persistLexemes

  lexemeToPayload :: EpicPhoneme o -> o
  lexemeToPayload  EpicPhonemeSilent = error "lexemeToPayload given silence"
  lexemeToPayload (EpicPhonemeFor _) = error "lexemeToPayload given a duration"
  lexemeToPayload (EpicPhonemeOnce m) = m
  lexemeToPayload (EpicPhonemeNewPersist m) = m
