module Sound.Tidal.Epic.Parse.File (
  Line
  , line
  , ignorable
  , emptyLine
  , comment
  , start
  , more
  , hsToGhci
  , readHsAsGhci
  ) where

import           Control.Applicative
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
  (satisfy, string, char, space, space1, anyChar, tab, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Sound.Tidal.Epic.Parse.Util


-- | Parse multiple indented lines of a .hs file, for the :cmd directive
data Line = Ignore | Start String | More String deriving Show

line = foldl1 (<|>) $ map try [emptyLine,comment,start,more]

ignorable :: Line -> Bool
ignorable Ignore = True
ignorable _ = False

emptyLine :: Parser Line
emptyLine = space >> eof >> return Ignore

comment :: Parser Line
comment = space >> satisfy (== '-') >> satisfy (== '-')
  >> skipMany anyChar >> eof >> return Ignore

start :: Parser Line
start = do c <- satisfy (/= ' ')
           rest <- many anyChar
           return $ Start $ c : rest

more :: Parser Line
more = do c <- satisfy (== ' ')
          rest <- many anyChar
          return $ More $ c : rest

hsToGhci :: String -> Either (ParseError (Token String) Void) String
hsToGhci s = do s1 <- mapM (parse line "") $ lines s
                let s2 = filter (not . ignorable) s1
                    f (Start s) = [":}",":{",s]
                    f (More s) = [s]
                    s3 = concatMap f s2
                return $ unlines $ tail s3 ++ [":}"]

-- | use this like `:cmd readHsAsGhci "folder/filename.hs"`
-- or better yet, make a macro: `:def! . readHsAsGhci`
-- and then call it like this: `:. folder/file.hs`
-- (Note that no quotation marks surround the filepath in the macro.)
readHsAsGhci :: FilePath -> IO String
readHsAsGhci filename = do
  s <- readFile filename
  case hsToGhci s of Left e -> (putStrLn $ show e) >> return ""
                     Right s -> return s
