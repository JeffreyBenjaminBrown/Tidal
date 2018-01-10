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
import           Sound.Tidal.Epic.Parse.Eq
import           Sound.Tidal.Epic.Transform
import           Sound.Tidal.Epic.Parse.Expr (parseEpicExpr)
import qualified Sound.Tidal.Epic.Parse.ParamMap as PM
import qualified Sound.Tidal.Epic.Parse.Scale    as Sc
import           Sound.Tidal.Epic.Parse.Transform (scanLang, cmdToAccumEpic)
import           Sound.Tidal.Epic.Parse.Util


_p :: ((Time -> i -> Epic i) -> Parser [EpicOrOp i])
   -> (Time -> i -> Epic i) -> String -> Epic i
_p p loopx s = case parse (sc >> p loopx) "" s of
  Left e -> error $ show e
  Right r -> case parse parseEpicExpr "" r of
    Left e -> error "unshowable Epic ParamMap parse error"
    Right r -> r
pe,pe0 :: String -> Epic ParamMap
pe  = _p peEpicOrOps loopa
pe0 = _p peEpicOrOps loop0 
ps  = _p psEpicOrOps loopa
ps0 = _p psEpicOrOps loop0 

pEpicOrOps :: (Monoidoid i o, Ord o) =>
  Parser [Lang i o] -> (Time -> i -> Epic i) -> Parser [EpicOrOp i]
pEpicOrOps p loopx = scanLang loopx <$> p
peEpicOrOps ::
  (Time -> ParamMap -> Epic ParamMap) -> Parser [EpicOrOp ParamMap]
peEpicOrOps = pEpicOrOps peLang
psEpicOrOps :: (Time -> Scale -> Epic Scale) -> Parser [EpicOrOp Scale]
psEpicOrOps = pEpicOrOps psLang

pLang :: (Monoidoid i o, Ord o) => Parser [Cmd i o] -> Parser [Lang i o]
pLang p = map f <$> p where
  f c = case c of
    CmdEpics list -> LangEpic $ cmdToAccumEpic $ S.fromList list
    CmdNonEpic nonEpic -> LangNonEpic nonEpic
peLang :: Parser [Lang ParamMap ParamMap]
peLang = pLang peCmds
psLang :: Parser [Lang Scale (Maybe Scale)]
psLang = pLang psCmds

pCmds :: Monoidoid i o => Parser [Cmd i o] -> Parser [Cmd i o]
pCmds p = concat <$> some f where f = p <|> (:[]) <$> pCmdCmdNonEpic
peCmds :: Parser [Cmd ParamMap ParamMap]
peCmds = pCmds peCmdEpics
psCmds :: Parser [Cmd Scale (Maybe Scale)]
psCmds = pCmds psCmdEpics

pCmdEpics :: Monoidoid i o => (Parser (EpicLexeme o)) -> Parser [Cmd i o]
pCmdEpics p = sepBy1 (CmdEpics <$> some p) (lexeme $ string ",,")
  -- TODO ? ',,' cannot yet be used like other binary operators;
  -- if one of its arguments is bracketed, it fails
peCmdEpics :: Parser [Cmd ParamMap ParamMap]
peCmdEpics = pCmdEpics PM.epicLexeme
psCmdEpics :: Parser [Cmd Scale (Maybe Scale)]
psCmdEpics = pCmdEpics Sc.epicLexeme


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
