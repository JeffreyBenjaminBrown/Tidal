-- >>> TODO: Brackets (modeled by Functions.hs)

-- -- Demonstration:
-- f = loope 0.2 can -- a 0.2-measure loop of the can sample
-- u = fast 2        -- (u f) is a 0.1-measure loop of the can sample
-- b = (+-)          -- concatenation
-- stream = [epicNotOp' f, binaryOp' b, unaryOp' u, epicNotOp' f]
--   -- = f `b` u f (because unary ops bind before binary ones)
--   -- = a 0.3 second loop of two can samples, one twice as long as the other
-- Right g = parse parseEpicOrOps "" stream
-- v1 g -- if v1 is a voice, then this will make that sound

{-# LANGUAGE TypeFamilies
           , FlexibleInstances #-}

module Sound.Tidal.Epic.Parse.EpicOrOp
  (parseEpicOrOps, EpicOrOp, epicNotOp', unaryOp', binaryOp')
where

import           Data.List (foldl')
import           Data.Proxy (Proxy(..))
import           Data.Semigroup ((<>))
import           Data.Void (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char (satisfy)
import           Text.Megaparsec.Expr (makeExprParser, Operator(..))
import           Text.Megaparsec.Pos

import           Sound.Tidal.Epic.Types


-- | = Types
newtype EpicWrap a = EpicWrap (Epic a)
newtype UnaryWrap a = UnaryWrap (Epic a -> Epic a)
newtype BinaryWrap a = BinaryWrap (Epic a -> Epic a -> Epic a)

data EpicOrOp a = EpicNotOp (EpicWrap a)
                | UnaryOp (UnaryWrap a)
                | BinaryOp (BinaryWrap a) deriving (Eq, Ord)

type Parser a = Parsec Void [EpicOrOp a]


-- | = Parsing
parseEpicOrOps :: Parser a (Epic a)
parseEpicOrOps = makeExprParser epicNotOp
  [ [ Prefix unaryOp ]
  , [ InfixL binaryOp ]
  ]

epicNotOp :: Parser a (Epic a)
epicNotOp = do EpicNotOp (EpicWrap f) <- satisfy isEpicNotOp
               return f

unaryOp :: Parser a (Epic a -> Epic a)
unaryOp = do UnaryOp (UnaryWrap op) <- satisfy isUnaryOp
             return op

binaryOp :: Parser a (Epic a -> Epic a -> Epic a)
binaryOp = do BinaryOp (BinaryWrap op) <- satisfy isBinaryOp
              return op


-- | = Basic type manipulations
isEpicNotOp, isUnaryOp, isBinaryOp :: EpicOrOp a -> Bool
isEpicNotOp (EpicNotOp _) = True
isEpicNotOp _ = False
isUnaryOp (UnaryOp _) = True
isUnaryOp _ = False
isBinaryOp (BinaryOp _) = True
isBinaryOp _ = False

epicNotOp' :: Epic a                   -> EpicOrOp a
epicNotOp' = EpicNotOp . EpicWrap
unaryOp'   :: (Epic a -> Epic a)         -> EpicOrOp a
unaryOp' = UnaryOp . UnaryWrap
binaryOp'  :: (Epic a -> Epic a -> Epic a) -> EpicOrOp a
binaryOp' = BinaryOp . BinaryWrap


-- | = Instances, and things only used for instances
-- The instances for Eq and Ord are dumb, just enough to enable parsing.
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
