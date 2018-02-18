module Sound.Tidal.Vector.Test where

import Test.HUnit
import Sound.Tidal.Vector.Test.DurVec
import Sound.Tidal.Vector.Test.VecEv

main = runTestTT $ TestList $ testDurVec ++ testVecEv
