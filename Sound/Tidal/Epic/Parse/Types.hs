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


-- Type variables `i` and `o` = "inner" and "outer"

data Timed o = Timed { timedDur :: Dur
                     , timedPayload :: o} deriving Eq

-- | = Lexeme: Parsing starts here
data NonEpicLexeme i = NonEpicLexemeUnOp (Epic i -> Epic i)
                     | NonEpicLexemeBinOp (Epic i -> Epic i -> Epic i)
                     | NonEpicLexemeLeftBracket | NonEpicLexemeRightBracket

-- | EpicPhonemes abut each other in a LexemeEpic
data EpicPhoneme o = EpicPhonemeFor     Dur
                   | EpicPhonemeOnce    o
                   | EpicPhonemeNewPersist o -- ^ these get merged with
                     -- persistentLexemes from earlier LexemeEpics
                   | EpicPhonemeSilent
                   deriving (Show, Eq, Ord)

data Lexeme i o = LexemeEpics [EpicPhoneme o]
                | LexemeNonEpic (NonEpicLexeme i) deriving Show


-- | = Lang: parsing reaches this once LexemeEpics are accumulated

-- | An AccumEpic in a list relies on earlier ones for meaning.
data AccumEpic o = AccumEpic -- ^ o is usually Map, esp. ParamMap
  { accumEpicDur     :: Maybe Dur
  , accumEpicTemp    :: o -- ^ applies only to the current AccumEpic
  , accumEpicPersist :: o -- ^ applies now and carries to the next
  , accumEpicSilent  :: Bool -- ^ except: if True, neither `o` applies now
                               -- (The persistent `o` still persists.)
  } deriving (Show, Eq, Ord)

data Lang i o = LangEpic (AccumEpic o)
              | LangNonEpic (NonEpicLexeme i) deriving Show

-- | = EpicOrOp: the last parsing stage before the resulting Epic
newtype EpicWrap a = EpicWrap (Epic a)
newtype UnaryWrap a = UnaryWrap (Epic a -> Epic a)
newtype BinaryWrap a = BinaryWrap (Epic a -> Epic a -> Epic a)

data EpicOrOp a = EpicNotOp (EpicWrap a)
                | UnaryOp (UnaryWrap a)
                | BinaryOp (BinaryWrap a)
                | LeftBracket | RightBracket deriving (Show, Eq, Ord)


-- | == Classes, Instances
instance Show (EpicWrap a) where show _ = "<EpicWrap>"
instance Show (UnaryWrap a) where show _ = "<UnaryWrap>"
instance Show (BinaryWrap a) where show _ = "<BinaryWrap>"
instance Show (NonEpicLexeme i) where
  show (NonEpicLexemeUnOp _) =  "<NonEpicLexemeUnOp>"
  show (NonEpicLexemeBinOp _) = "<NonEpicLexemeBinOp>"
  show NonEpicLexemeLeftBracket = "["
  show NonEpicLexemeRightBracket = "]"

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

type Transform a = Epic a -> Epic a

instance Monoidoid (Transform a) (Transform a) where
  mempty' = id
  mappend' = (.) -- ^ is never used, because all phonemes are EpicPhoneOnces
  null' = const False -- ^ tricky : the use of null' is to replace a null
    -- expression with silence. (It is only used once, in Parse.Transform.)
    -- In this case, a transform (an Epic a -> Epic a) can already create
    -- silence, so no special handling is needed.
  unwrap = id

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
