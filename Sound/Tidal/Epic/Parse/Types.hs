{-# LANGUAGE TypeFamilies
           , FlexibleInstances #-}

module Sound.Tidal.Epic.Parse.Types where

import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Proxy (Proxy(..))
import           Data.Semigroup ((<>))
import           Text.Megaparsec
import           Text.Megaparsec.Pos

import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types


-- | An AccumEpicLang in a list relies on earlier ones for meaning.
data AccumEpicLang t = AccumEpicLang -- ^ t is usually Map, esp. ParamMap
  { accumLangDur     :: Maybe Dur
  , accumLangTemp    :: t -- ^ applies only to the current AccumEpicLang
  , accumLangPersist :: t -- ^ applies now and carries to the next
  , accumLangSilent  :: Bool -- ^ except: if True, neither t applies now
                               -- (The persistent one still persists.)
  } deriving (Show, Eq, Ord)

-- | Like AccumEpicLang, but context-free: A DurMonoid in a list
-- does not rely on earlier DurMonoids for meaning.
data DurMonoid t = DurMonoid { durMonoidDur :: Dur
                             , durMonoid :: t} deriving Eq

-- >>> TODO ? Subsume these constructors into AccumLang and EpicOrOpIsh
data EpicOp t = CmdOpUnary  (Epic t -> Epic t)
              | CmdOpBinary (Epic t -> Epic t -> Epic t)

-- | Almost an EpicOrOp; just needs scan-accumulation
data AccumLang t = CxCmdMap (AccumEpicLang t)
               | CxCmdOp (EpicOp t)
               | CxLeftBracket2s | CxRightBracket2s
               -- todo: remove the 2s once the namespace is free.

-- TODO: unify with EpicOrOp
data EpicOrOpIsh t = CmdMap (DurMonoid t)
                   | CmdOp (EpicOp t)
                   | LeftBracket2s | RightBracket2s
                   -- todo: remove the 2s once the namespace is free.

-- | = EpicOrOp
newtype EpicWrap a = EpicWrap (Epic a)
newtype UnaryWrap a = UnaryWrap (Epic a -> Epic a)
newtype BinaryWrap a = BinaryWrap (Epic a -> Epic a -> Epic a)

data EpicOrOp a = EpicNotOp (EpicWrap a)
                | UnaryOp (UnaryWrap a)
                | BinaryOp (BinaryWrap a)
                | LeftBracket | RightBracket deriving (Eq, Ord)


-- | == Instances
class Monoidoid a where mempty' :: a
                        mappend' :: a -> a -> a
                        null' :: a -> Bool

instance Ord a => Monoidoid (M.Map a b) where mempty' = M.empty
                                              mappend' = M.union
                                              null' = M.null

instance Monoidoid (Maybe a) where mempty' = Nothing
                                   mappend' Nothing Nothing = Nothing
                                   mappend' Nothing b       = b
                                   mappend' a       b       = a
                                   null' = isNothing

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
