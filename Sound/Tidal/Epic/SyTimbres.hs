module Sound.Tidal.Epic.SyTimbres where

import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.Parse.Lexeme

syFuzzBass  = pe "_sy,sus1,f110" &* pe "f1,,,,ff0.25 | f2,g0.7,pa1,pf0.25"
syFuzzBass'  = pe "_sy,sus1     " &* pe "f1,,,,ff0.25 | f2,g0.7,pa1,pf0.25"
syHeavyBass = pe "_sy,sus1,f55"  &* pe "f1,fa1,ff0.5 | f2,g0.7,pa1,pf0.25"
syHeavyBass' = pe "_sy,sus1    "  &* pe "f1,fa1,ff0.5 | f2,g0.7,pa1,pf0.25"
syHollowBass = syFuzzBass' &* pe "f100.5,sus0.5,g1.2,pa0.01,pf13 | pa0.02,pf7"
