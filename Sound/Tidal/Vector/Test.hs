module Sound.Tidal.Vector.Test where

import qualified Data.Vector as V
import Test.HUnit

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Vector.Types 

main = runTestTT $ TestList
  [ TestLabel "testVecDur" testVecDur
  ]

ve start duration payload =
  VecEv {start = start, duration = duration, payload = payload}
j = Just
n = Nothing

testVecDur = TestCase $ do
  let dv = V.fromList [ve 0 10 $ j 'a', ve 10 20 $ j 'b']
      dv0 = V.fromList []
  assertBool "0" $ vecDur dv == 0
  assertBool "1" $ vecDur dv == 30
