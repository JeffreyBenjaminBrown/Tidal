module Sound.Tidal.Epic.Parse.SeqCommand2Stage where

import qualified Data.Set as S
import           Data.Map (Map(..))
import qualified Data.Map as M
import           Data.Maybe (isNothing)

import           Sound.Tidal.Epic.Abbreviations
import           Sound.Tidal.Epic.Params
import           Sound.Tidal.Epic.Transform (durSilence)
import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import qualified Sound.Tidal.Params      as P


-- -- | A Cmd2s from which a (melodic) sequence is generated.
-- -- Some commands only apply once, to the current block.
-- -- Others apply to this one and the succeeding ones, until overridden.
-- data Cmd2s = CmdDur2s          Dur
--            | CmdParamPersist2s ParamMap
--            | CmdParamOnce2s    ParamMap
--            | CmdSilent2s
--            deriving (Show, Eq, Ord)

-- -- todo ? this should process a list, one elt at a time,
-- -- using pattern matching, instead of this unreadable idiom.
-- toCmdBlock :: S.Set Cmd -> CmdBlock
-- toCmdBlock s = CmdBlock dur silent persist once where
--   (durCmds, nonDurCmds) = S.partition isDur s
--   (silentCmds, paramCmds) = S.partition isSilent nonDurCmds
--   (onceCmds, persistCmds) = S.partition isOnce paramCmds
--   isDur, isSilent, isOnce :: Cmd -> Bool
--   isDur (CmdDur _)        = True; isDur _    = False
--   isSilent CmdSilent      = True; isSilent _ = False
--   isOnce (CmdParamOnce _) = True; isOnce _   = False
--   dur = case S.toList durCmds of
--     (CmdDur t):_ -> Just t
--     _            -> Nothing
--   silent = if S.null silentCmds then False else True
--   once = M.unions $ fromCmdParam <$> S.toList onceCmds
--   persist = M.unions $ fromCmdParam <$> S.toList persistCmds
--   fromCmdParam :: Cmd -> ParamMap
--   fromCmdParam (CmdDur _) = error "fromCmdParam given a duration"
--   fromCmdParam CmdSilent = M.empty -- nor should this happen
--   fromCmdParam (CmdParamOnce m) = m
--   fromCmdParam (CmdParamPersist m) = m


-- = todo ? split module: The Cmd2s type appears below and not above.


-- >>> TODO: Define newtypes to make monoids out of "a" and "Map a b".
-- Maps are not Monoids, so use a newtype, and define (<>) = Map.union.
-- For general values (e.g. scales), wrap in Maybe, define <> = const <$>?

class Monoid' a where mempty' :: a
                      mappend' :: a -> a -> a
                      null' :: a -> Bool

instance Ord a => Monoid' (Map a b) where mempty' = M.empty
                                          mappend' = M.union
                                          null' = M.null

instance Monoid' (Maybe a) where mempty' = Nothing
                                 mappend' Nothing Nothing = Nothing
                                 mappend' Nothing b = b
                                 mappend' a b = a
                                 null' = isNothing

-- | A CxDurMonoid in a list of them relies on the earlier ones for meaning.
-- Cx = "context". "Monoid" because t must be a Monoid' for
-- cxDurScanAccum to work; it's the whole point of this type.
data CxDurMonoid t = CxDurMonoid -- ^ t is usually Map, esp. ParamMap
  { cxDurMonoidDur     :: Maybe Dur
  , cxDurMonoidTemp    :: t -- ^ applies only to the current CxDurMonoid
  , cxDurMonoidPersist :: t -- ^ applies now and carries to the next
  , cxDurMonoidSilent  :: Bool -- ^ except: if True, neither t applies now
                               -- (The persistent one still persists.)
  } deriving (Show, Eq, Ord)

-- | Like CxDurMonoid, but context-free.
-- An DurMonoid in a list does not rely on earlier DurMonoids for meaning.
data DurMonoid t = DurMonoid { durMonoidDur :: Maybe Dur
                             , durMonoid  :: t}

data EpicOp t = CmdOpUnary  (Epic t -> Epic t)
              | CmdOpBinary (Epic t -> Epic t -> Epic t)

data Cmd2s t = CmdMap (DurMonoid t)
             | CmdOp (EpicOp t)
             | LeftBracket2s | RightBracket2s
             -- >>> TODO: remove the 2s once the namespace is free.

-- | ASSUMES: Until a duration is specified, duration defaults to 1.
-- I called this "scanAccum" by analogy with scanl and mapAccum:
-- mapAccum is to map as scanAccum is to scanl.

cxDurScanAccum :: Monoid' t => [CxDurMonoid t] -> [(Dur, t)]
cxDurScanAccum bs = _cxDurScanAccum (1, mempty') bs

-- | TODO >>> Use maybe rather than mempty.
_cxDurScanAccum :: Monoid' t => (Dur, t) -> [CxDurMonoid t] -> [(Dur, t)]
_cxDurScanAccum priorPersistentCmds [] = []
_cxDurScanAccum (prevDur, prevMap) (CxDurMonoid mdur persist once sil : bs) =
  let next = mappend' persist prevMap
      now = foldl1 mappend' [once, persist, prevMap]
      nowDur = maybe prevDur id mdur
      ignoreIfSilent :: Monoid' t => t -> t
      ignoreIfSilent m = if sil then mempty' else m
  in (nowDur, ignoreIfSilent now)
     : _cxDurScanAccum (nowDur, next) bs

-- blockToEpic0, blockToEpic :: (Dur, t) -> Epic t
-- blockToEpic0 (dur, map) =
--   if M.null map then durSilence dur else loop0 dur map
-- blockToEpic  (dur, map) =
--   if M.null map then durSilence dur else loopa dur map

-- blocksToEpic0, blocksToEpic :: [CmdBlock] -> ParamEpic
-- blocksToEpic0 = foldl1 (+-) . map blockToEpic0 . cxDurScanAccum
-- blocksToEpic  = foldl1 (+-) . map blockToEpic  . cxDurScanAccum
