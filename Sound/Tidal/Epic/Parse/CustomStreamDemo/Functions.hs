-- Demonstration:
-- > f a = a + 1              -- below I will (polymorphically) call f "+1"
-- > u f = \a -> f a * 2
-- > b f g = \a -> f a + g a  -- below I will (polymorphically) call b "+"
-- > stream = [FuncNotOp f, BinaryOp b, UnaryOp u, FuncNotOp f]
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


func :: Parser Func
func = makeExprParser funcNotOp
  [ [ Prefix unaryOp ]
  , [ InfixL binaryOp ]
  ]

funcNotOp :: Parser Func
funcNotOp = do FuncNotOp f <- satisfy isFuncNotOp
               return f

unaryOp :: Parser (Func -> Func)
unaryOp = do UnaryOp op <- satisfy isUnaryOp
             return op

binaryOp :: Parser (Func -> Func -> Func)
binaryOp = do BinaryOp op <- satisfy isBinaryOp
              return op


-- | = The Func (think of it as Epic) type.
-- Its instances for Enum, Eq and Ord are dumb, just enough to enable parsing.
type Func = Float -> Float

isFuncNotOp, isUnaryOp, isBinaryOp :: FuncOrOp -> Bool
isFuncNotOp (FuncNotOp _) = True
isFuncNotOp _ = False
isUnaryOp (UnaryOp _) = True
isUnaryOp _ = False
isBinaryOp (BinaryOp _) = True
isBinaryOp _ = False

-- | TODO: Use newtypes for these instances.
instance Eq Func                     where (==) _ _ = False
instance Eq (Func -> Func)           where (==) _ _ = False
instance Eq (Func -> Func -> Func)   where (==) _ _ = False
instance Ord Func                    where (<=) _ _ = False
instance Ord (Func -> Func)          where (<=) _ _ = False
instance Ord (Func -> Func -> Func)  where (<=) _ _ = False


-- | = The FuncOrOp (think EpicOrOp) type -- the thing to parse.
data FuncOrOp = FuncNotOp Func
              | UnaryOp (Func -> Func)
              | BinaryOp (Func -> Func -> Func) deriving (Eq, Ord)

type Parser = Parsec Void [FuncOrOp]

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
