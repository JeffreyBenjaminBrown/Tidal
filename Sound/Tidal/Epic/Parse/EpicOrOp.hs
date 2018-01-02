-- For a demonstration, see EpicOrOp.test.hs, but first search this file
-- for "EpicOrOp.test.hs" and uncomment the line below it.

module Sound.Tidal.Epic.Parse.EpicOrOp
  ( parseEpicExpr
  , EpicOrOp(LeftBracket, RightBracket) -- The other constructors
    -- (EpicNotOp, UnaryOp, BinaryOp) require the non-exported *Wrap types.
    -- Instead of those constructors, use epicNotOp, unaryOp, binaryOp.
  , epicNotOp, unaryOp, binaryOp

  -- | = These I only export when running EpicOrOp.test.hs
  -- , parseEpic, parseUnaryOp, parseBinaryOp
  ) where

import           Data.Void (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char (satisfy)
import           Text.Megaparsec.Expr (makeExprParser, Operator(..))

import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Parse.Types


-- | = Parsing
type Parser a = Parsec Void [EpicOrOp a]

parseEpicExpr :: Parser a (Epic a)
parseEpicExpr = makeExprParser parseEpic [ [ Prefix parseUnaryOp ]
                                         , [ InfixL parseBinaryOp ]
                                         ]

parseEpic :: Parser a (Epic a)
parseEpic = it <|> bracket parseEpicExpr
  where it = do EpicNotOp (EpicWrap f) <- satisfy isEpicNotOp
                return f

parseUnaryOp :: Parser a (Epic a -> Epic a)
parseUnaryOp = it <|> bracket parseUnaryOp
  where it = do UnaryOp (UnaryWrap op) <- satisfy isUnaryOp
                return op

parseBinaryOp :: Parser a (Epic a -> Epic a -> Epic a)
parseBinaryOp = it <|> bracket parseBinaryOp
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

epicNotOp :: Epic a                       -> EpicOrOp a
epicNotOp = EpicNotOp . EpicWrap
unaryOp   :: (Epic a -> Epic a)           -> EpicOrOp a
unaryOp   = UnaryOp   . UnaryWrap
binaryOp  :: (Epic a -> Epic a -> Epic a) -> EpicOrOp a
binaryOp  = BinaryOp  . BinaryWrap
