-- For a demonstration, see EpicOrOp.test.hs, but first search this file
-- for "EpicOrOp.test.hs" and uncomment the line below it.

{-# LANGUAGE TypeFamilies
           , FlexibleInstances #-}

module Sound.Tidal.Epic.Parse.EpicOrOp
  (parseEpicOrOps
  , EpicOrOp(..) -- ^ This only makes LeftBracket and RightBracket available.
    -- The other constructors rely on the *Wrap types, which are not exported.
    -- Instead of those, use epicNotOp', unaryOp', binaryOp'.
  , epicNotOp', unaryOp', binaryOp'

  -- | = These I only export when running EpicOrOp.test.hs
  -- , epicNotOp, unaryOp, binaryOp
  )
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
                | BinaryOp (BinaryWrap a)
                | LeftBracket | RightBracket deriving (Eq, Ord)

type Parser a = Parsec Void [EpicOrOp a]


-- | = Parsing
parseEpicOrOps :: Parser a (Epic a)
parseEpicOrOps = makeExprParser epicNotOp [ [ Prefix unaryOp ]
                                          , [ InfixL binaryOp ]
                                          ]

epicNotOp :: Parser a (Epic a)
epicNotOp = it <|> bracket parseEpicOrOps
  where it = do EpicNotOp (EpicWrap f) <- satisfy isEpicNotOp
                return f

unaryOp :: Parser a (Epic a -> Epic a)
unaryOp = it <|> bracket unaryOp
  where it = do UnaryOp (UnaryWrap op) <- satisfy isUnaryOp
                return op

binaryOp :: Parser a (Epic a -> Epic a -> Epic a)
binaryOp = it <|> bracket binaryOp
  where it = do BinaryOp (BinaryWrap op) <- satisfy isBinaryOp
                return op

bracket :: Parser a x -> Parser a x
bracket = try . between (satisfy isLeftBracket) (satisfy isRightBracket)

-- | = Basic type manipulations
isEpicNotOp, isUnaryOp, isBinaryOp :: EpicOrOp a -> Bool
isEpicNotOp (EpicNotOp _) = True   ; isEpicNotOp _ = False
isUnaryOp (UnaryOp _) = True       ; isUnaryOp _ = False
isBinaryOp (BinaryOp _) = True     ; isBinaryOp _ = False
isLeftBracket LeftBracket = True   ; isLeftBracket _ = False
isRightBracket RightBracket = True ; isRightBracket _ = False

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
