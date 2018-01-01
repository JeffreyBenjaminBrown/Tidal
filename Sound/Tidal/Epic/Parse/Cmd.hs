-- Pitfall: With OverloadedStrings, `parse` needs a type signature
-- to specify that the last argument is a String.

module Sound.Tidal.Epic.Parse.Cmd where

import           Control.Applicative
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Void (Void)
import           GHC.Exts( IsString(..) )
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Util (Parser(..)
  , ratio, lexeme, sc, anyWord, double, ignore)
import Sound.Tidal.Epic.Parse.SeqCommand (Cmd(..), CmdBlock(..)
  , blocksToEpic, blocksToEpic0, toCmdBlock)


parseParamMapSeq, parseParamMapSeq0, p, p0 :: String -> ParamEpic
p  = parseParamMapSeq
p0 = parseParamMapSeq0
parseParamMapSeq  s = let x = parse paramMapSeq "" s
  in case x of Right pe -> pe
               Left err -> error $ show err
parseParamMapSeq0 s = let x = parse paramMapSeq0 "" s
  in case x of Right pe -> pe
               Left err -> error $ show err

-- | == parse a sequence of ParamMaps as an Epic
paramMapSeq, paramMapSeq0 :: Parser ParamEpic
paramMapSeq  = sc >> blocksToEpic  <$> sepBy1 cmdBlock (lexeme $ string ",,")
paramMapSeq0 = sc >> blocksToEpic0 <$> sepBy1 cmdBlock (lexeme $ string ",,")

cmdBlock :: Parser CmdBlock
cmdBlock = toCmdBlock . S.fromList <$> some cmd

-- | == parse Cmds 
-- (s)peed, scale (d)egree, dura(t)ion, (g)ain
-- to play silence, use "_"; to play a sample or synth, use "_name"
cmd, cmdParamPersist, cmdParamOnce, cmdDuration :: Parser Cmd
cmd = foldl1 (<|>) [cmdParamPersist, cmdParamOnce, cmdDuration, cmdSilence]
cmdParamPersist = lexeme $ CmdParamPersist <$> parseSingleton
cmdParamOnce = lexeme $ CmdParamOnce <$> (ignore (char '1') >> parseSingleton)
cmdDuration = lexeme $ ignore (char 't') >> CmdDur <$> ratio
cmdSilence = lexeme $ const CmdSilent <$> char '_'

-- | == parse ParamMaps
parseSingleton :: Parser ParamMap
parseSingleton = foldl1 (<|>) $ map try
  [parseSpeed, parseGain, parseSound, parseDegree, parseSustain
  , parseQf, parseQfa, parseQff, parseQpa, parseQpf, parseQaa, parseQaf ]

parseSpeed, parseGain, parseSound, parseDegree :: Parser ParamMap
parseSpeed   = parseSingletonFloat  speed_p   $ ignore $ char 's'
parseGain    = parseSingletonFloat  gain_p    $ ignore $ char 'g'
parseSound   = parseSingletonString sound_p   $ ignore $ char '_'
parseDegree  = parseSingletonFloat  deg_p     $ ignore $ char 'd'
parseSustain = parseSingletonFloat  sustain_p $ ignore $ string "sus"
parseQf      = parseSingletonFloat  qf_p      $ ignore $ string "f"
parseQfa     = parseSingletonFloat  qfa_p     $ ignore $ string "fa"
parseQff     = parseSingletonFloat  qff_p     $ ignore $ string "ff"
parseQpa     = parseSingletonFloat  qpa_p     $ ignore $ string "pa"
parseQpf     = parseSingletonFloat  qpf_p     $ ignore $ string "pf"
parseQaa     = parseSingletonFloat  qaa_p     $ ignore $ string "aa"
parseQaf     = parseSingletonFloat  qaf_p     $ ignore $ string "af"

parseSingletonFloat :: Param -> Parser () -> Parser ParamMap
parseSingletonFloat param prefix =
  M.singleton param . VF <$> (prefix >> double)

parseSingletonString :: Param -> Parser () -> Parser ParamMap
parseSingletonString param prefix =
  M.singleton param . VS <$> (prefix >> anyWord)
