module Sound.Tidal.Epic.Parse.Util where

import           Control.Applicative
import           Data.Ratio
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec as Megp
import           Text.Megaparsec.Char (
  satisfy, string, char, space, space1, anyChar, tab
  , alphaNumChar, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

ignore :: Parser a -> Parser ()
ignore p = p >> return ()

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Integral a => Parser a
integer = L.decimal

double :: Parser Double
double = realToFrac <$> L.signed (return ()) L.scientific
  -- Here it's safe (and avoids another dependency), but in general, note:
  -- "Avoid applying toRational (or realToFrac) to scientific numbers coming from an untrusted source and use toRealFloat instead. The latter guards against excessive space usage." https://hackage.haskell.org/package/scientific-0.3.5.2/docs/Data-Scientific.html

ratio :: Integral a => Parser (Ratio a)
ratio = do a <- L.decimal
           b <- (try $ char '%' >> L.decimal) <|> return 1
           return $ a % b

-- | For words, it's better to use `word`.
-- `symbol` doesn't check whether it's followed by a word character.
-- For instance, `parse (symbol "a") "" "aardvark"` will parse an "a"
-- when failure would be more natural.
symbol :: String -> Parser String
symbol = L.symbol sc

-- TODO: Mystery: I want wordChar to include '_', but it makes tests fail.
wordChar :: Parser Char
wordChar = alphaNumChar <|> char '-'

-- | The notFollowedBy makes word different from symbol.
-- For instance, word "(" will fail where symbol "(" would not.
word :: String -> Parser String -- | could fail half-in, so requires "try"
word w = string w <* notFollowedBy wordChar

anyWord :: Parser String
anyWord = Megp.some wordChar <* notFollowedBy wordChar

anyDigitlessWord :: Parser String
anyDigitlessWord = Megp.some letterChar  <* notFollowedBy letterChar
