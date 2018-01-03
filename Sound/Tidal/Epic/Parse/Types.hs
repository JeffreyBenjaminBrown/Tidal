{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
#-}

module Sound.Tidal.Epic.Parse.Types where

import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Maybe (isNothing, fromJust)
import           Data.Proxy (Proxy(..))
import           Data.Semigroup ((<>))
import           Text.Megaparsec
import           Text.Megaparsec.Pos

import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types


-- | = type variables `i` and `o` = "inner" and "outer"

data Timed o = Timed { timedDur :: Dur
                     , timedPayload :: o} deriving Eq

-- | An AccumEpicLang in a list relies on earlier ones for meaning.
data AccumEpicLang o = AccumEpicLang -- ^ o is usually Map, esp. ParamMap
  { accumLangDur     :: Maybe Dur
  , accumLangTemp    :: o -- ^ applies only to the current AccumEpicLang
  , accumLangPersist :: o -- ^ applies now and carries to the next
  , accumLangSilent  :: Bool -- ^ except: if True, neither `o` applies now
                               -- (The persistent `o` still persists.)
  } deriving (Show, Eq, Ord)

-- | Almost an EpicOrOp; just needs scan-accumulation
data AccumLang i o = AccumLangTerm (AccumEpicLang o)
  | AccumLangUnOp (Epic i -> Epic i)
  | AccumLangBinOp (Epic i -> Epic i -> Epic i)
  | AccumLangLeftBracket | AccumLangRightBracket

-- | = EpicOrOp
newtype EpicWrap a = EpicWrap (Epic a)
newtype UnaryWrap a = UnaryWrap (Epic a -> Epic a)
newtype BinaryWrap a = BinaryWrap (Epic a -> Epic a -> Epic a)

data EpicOrOp a = EpicNotOp (EpicWrap a)
                | UnaryOp (UnaryWrap a)
                | BinaryOp (BinaryWrap a)
                | LeftBracket | RightBracket deriving (Eq, Ord)


-- | == Instances
class Monoidoid inner o | o -> inner where
  mempty' :: o
  mappend' :: o -> o -> o
  null' :: o -> Bool
  unwrap :: o -> inner -- could fail if null'

instance Ord a => Monoidoid (M.Map a b) (M.Map a b) where
  mempty' = M.empty
  mappend' = M.union -- from this comes the Ord requirement
  null' = M.null
  unwrap = id

instance Monoidoid i (Maybe i) where
  mempty' = Nothing
  mappend' Nothing Nothing = Nothing
  mappend' Nothing b       = b
  mappend' a       b       = a
  null' = isNothing
  unwrap = fromJust

-- | = These instances for Eq and Ord are dumb, just enough to enable parsing.
instance Eq (EpicWrap   a) where (==) _ _ = False
instance Eq (UnaryWrap  a) where (==) _ _ = False
instance Eq (BinaryWrap a) where (==) _ _ = False

instance Ord (EpicWrap   a) where (<=) _ _ = False
instance Ord (UnaryWrap  a) where (<=) _ _ = False
instance Ord (BinaryWrap a) where (<=) _ _ = False

-- | nearly identical to "instance Stream String" from Text.Megaparsec.Stream
instance Stream [EpicOrOp a] where
  type Token [EpicOrOp a] = EpicOrOp a -- ^ one difference
  type Tokens [EpicOrOp a] = [EpicOrOp a] -- ^ another difference
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s) -- ^ one more difference ([] instead of "")
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

defaultAdvance1 :: Pos               -- ^ Tab width
                -> SourcePos         -- ^ Current position
                -> t                 -- ^ Current token
                -> SourcePos         -- ^ Incremented position
defaultAdvance1 _ (SourcePos n l c) _ = SourcePos n l $ c <> pos1
  -- this just adds one to c. l never advances to some putative next line.
