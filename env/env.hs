:set prompt "> "
:set -XOverloadedStrings

:m Sound.Tidal.Epic
:def! . readHsAsGhci
:s EpicDemo/MakeVoices.hs

import Control.Concurrent.MVar
import Control.Lens
import Data.Maybe
import Data.Map as M
import Data.Set as S
import Data.Ratio
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Sound.OSC.Transport.FD
-- import Sound.OSC.FD
import System.Environment (lookupEnv)
import Text.Megaparsec
-- import Text.Megaparsec.Char (satisfy, string, space, space1, anyChar, tab)
import qualified Text.Megaparsec.Char.Lexer as L
