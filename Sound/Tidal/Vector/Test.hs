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
  let dv0    = V.fromList []
      dv     = V.fromList [ve 0 10 $ j 'a', ve 10 20 $ j 'b']
      dvBad  = V.fromList [ve 0 10 $ j 'a', ve 05 20 $ j 'b']
  assertBool "0 length 0" $ vecDur dv0 == 0
  assertBool "0 legit" $ checkDurVec dv0
  assertBool "1 length 0" $ vecDur dv  == 30
  assertBool "1 legit" $ checkDurVec dv
  assertBool "2 not legit" $ not $ checkDurVec dvBad
