{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.Epic.Parse.Cmd where

import qualified Data.Set as S
import qualified Data.Map as M
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)

import           Sound.Tidal.Epic.Types.Reimports
import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Parse.Types

import           Sound.Tidal.Epic.CombineEpics
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Parse.Expr (parseEpicExpr)
import qualified Sound.Tidal.Epic.Parse.ParamMap as PM
import qualified Sound.Tidal.Epic.Parse.Scale    as Sc
import           Sound.Tidal.Epic.Parse.Transform (scanLang, cmdToAccumEpic)
import           Sound.Tidal.Epic.Parse.Util


_pEpic :: (Time -> ParamMap -> Epic ParamMap) -> String -> Epic ParamMap
_pEpic loopx s = case parse (sc >> pEpicOrOps loopx) "" s of
  Left e -> error $ show e
  Right r -> case parse parseEpicExpr "" r of
    Left e -> error "unshowable Epic ParamMap parse error"
    Right r -> r
pe,pe0 :: String -> Epic ParamMap
pe = _pEpic loopa
pe0 = _pEpic loop0

pEpicOrOps :: (Time -> ParamMap -> Epic ParamMap)
            -> Parser [EpicOrOp ParamMap]
pEpicOrOps loopx = scanLang loopx <$> pLang

pLang :: Parser [Lang ParamMap ParamMap]
pLang = map f <$> pmCmds where
  f c = case c of
    CmdEpics list -> LangEpic $ cmdToAccumEpic $ S.fromList list
    CmdNonEpic nonEpic -> LangNonEpic nonEpic

pmCmds :: Parser [Cmd ParamMap ParamMap]
pmCmds = concat <$> some f where f = pmCmdEpics <|> (:[]) <$> pCmdCmdNonEpic
--psCmds :: Parser [Cmd Scale Scale]
--psCmds = concat <$> some f where f = psCmdEpics <|> (:[]) <$> pCmdCmdNonEpic

pmCmdEpics :: Parser [Cmd ParamMap ParamMap]
pmCmdEpics = sepBy1 (CmdEpics <$> some PM.epicLexeme) (lexeme $ string ",,")
  -- TODO ? ',,' cannot yet be used like other binary operators;
  -- if one of its arguments is bracketed, it fails
psCmdEpics' :: Parser [Cmd Scale Scale]
psCmdEpics' = sepBy1 (CmdEpics <$> some Sc.epicLexeme) (lexeme $ string ",,")
  -- TODO ? ',,' cannot yet be used like other binary operators;
  -- if one of its arguments is bracketed, it fails


-- | = The code below does not depend on the payload (ParamMap, scale, etc.)
pCmdCmdNonEpic :: Parser (Cmd i o)
pCmdCmdNonEpic = CmdNonEpic <$> pLangNonEpic

pLangNonEpic, pLangNonEpicFast, pLangNonEpicStack, pLangNonEpicCat,
  pLangNonEpicLeftBracket, pLangNonEpicRightBracket :: Parser (LangNonEpic i)
pLangNonEpic = foldr1 (<|>) $ map try $ [ pLangNonEpicFast
                                        , pLangNonEpicStack
                                        , pLangNonEpicCat
                                        , pLangNonEpicLeftBracket
                                        , pLangNonEpicRightBracket
                                        ]
pLangNonEpicFast = do lexeme $ symbol "fast"
                      return $ LangNonEpicUnOp $ eFast 2
pLangNonEpicStack = do lexeme $ symbol "stack"
                       return $ LangNonEpicBinOp eStack
pLangNonEpicCat = do lexeme $ symbol "cat"
                     return $ LangNonEpicBinOp concatEpic
pLangNonEpicLeftBracket = do lexeme $ symbol "["
                             return LangNonEpicLeftBracket
pLangNonEpicRightBracket = do lexeme $ symbol "]"
                              return LangNonEpicRightBracket
