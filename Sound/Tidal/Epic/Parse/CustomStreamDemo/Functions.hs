-- Demonstration:
-- > f a = a + 1              -- below I will (polymorphically) call f "+1"
-- > u f = \a -> f a * 2
-- > b f g = \a -> f a + g a  -- below I will (polymorphically) call b "+"
-- > stream = [funcNotOp' f, binaryOp' b, unaryOp' u, funcNotOp' f]
-- >   -- = f `b` u f (because unary ops bind before binary ones)
-- >   -- = (+1) + u (+1)
-- >   -- = (+1) + \a -> (a+1)*2
-- >   -- = \a -> (a+1)*2 + (a+1)
-- >   -- = \a -> 3a + 3
-- > Right g = parse func "" stream
-- > g 0
-- 3.0
-- > g 1
-- 6.0
-- > g 2
-- 9.0

{-# LANGUAGE TypeFamilies
           , FlexibleInstances #-}

module Sound.Tidal.Epic.Parse.CustomStreamDemo.Functions where

import           Data.List (foldl')
import           Data.Proxy (Proxy(..))
import           Data.Semigroup ((<>))
import           Data.Void (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char (satisfy)
import           Text.Megaparsec.Expr (makeExprParser, Operator(..))
import           Text.Megaparsec.Pos


-- | = Types
type Func = Float -> Float

newtype FuncWrap = FuncWrap Func
newtype UnaryWrap = UnaryWrap (Func -> Func)
newtype BinaryWrap = BinaryWrap (Func -> Func -> Func)

data FuncOrOp = FuncNotOp FuncWrap
              | UnaryOp UnaryWrap
              | BinaryOp BinaryWrap
              | LeftBracket | RightBracket deriving (Eq, Ord)

type Parser = Parsec Void [FuncOrOp]


-- | = Parsing
funcExpr :: Parser Func
funcExpr = makeExprParser func [ [ Prefix unaryOp ]
                               , [ InfixL binaryOp ]
                               ]

func :: Parser Func
func = it <|> bracket funcExpr where
  it = do FuncNotOp (FuncWrap f) <- satisfy isFuncNotOp
          return f

unaryOp :: Parser (Func -> Func)
unaryOp = it <|> bracket unaryOp where
  it = do UnaryOp (UnaryWrap op) <- satisfy isUnaryOp
          return op

binaryOp :: Parser (Func -> Func -> Func)
binaryOp = it <|> bracket binaryOp 
  where it = do BinaryOp (BinaryWrap op) <- satisfy isBinaryOp
                return op

bracket :: Parser a -> Parser a
bracket p = try $ between (satisfy isLeftBracket) (satisfy isRightBracket) p


-- | = Basic type manipulations
isFuncNotOp, isUnaryOp, isBinaryOp :: FuncOrOp -> Bool
isFuncNotOp (FuncNotOp _) = True
isFuncNotOp _ = False
isUnaryOp (UnaryOp _) = True
isUnaryOp _ = False
isBinaryOp (BinaryOp _) = True
isBinaryOp _ = False
isLeftBracket LeftBracket = True
isLeftBracket _ = False
isRightBracket RightBracket = True
isRightBracket _ = False

funcNotOp' :: Func                   -> FuncOrOp
funcNotOp' = FuncNotOp . FuncWrap
unaryOp'   :: (Func -> Func)         -> FuncOrOp
unaryOp' = UnaryOp . UnaryWrap
binaryOp'  :: (Func -> Func -> Func) -> FuncOrOp
binaryOp' = BinaryOp . BinaryWrap


-- | = Instances, and things only used for instances
-- The instances for Eq and Ord are dumb, just enough to enable parsing.
instance Eq FuncWrap    where (==) _ _ = False
instance Eq UnaryWrap   where (==) _ _ = False
instance Eq BinaryWrap  where (==) _ _ = False

instance Ord FuncWrap   where (<=) _ _ = False
instance Ord UnaryWrap  where (<=) _ _ = False
instance Ord BinaryWrap where (<=) _ _ = False

-- | nearly identical to "instance Stream String" from Text.Megaparsec.Stream
instance Stream [FuncOrOp] where
  type Token [FuncOrOp] = FuncOrOp -- ^ one difference
  type Tokens [FuncOrOp] = [FuncOrOp] -- ^ another difference
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
