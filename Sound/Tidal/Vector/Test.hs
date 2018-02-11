module Sound.Tidal.Vector.Test where

import qualified Data.Vector as V
import Test.HUnit

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types 

ve start duration payload =
  VecEv {veStart = start, veDuration = duration, vePayload = payload}
j = Just
n = Nothing
m = n :: Maybe Int

main = runTestTT $ TestList
  [ TestLabel "testDvDur" testDvDur
  , TestLabel "testVecEv" testVecEv
  ]

testVecEv = TestCase $ do
  assertBool "1" $ sortDurVec (V.fromList [ve 10  2 m, ve 10 1 m, ve 1  20 m])
                            == V.fromList [ve 1  20 m, ve 10 1 m, ve 10  2 m]

testDvDur = TestCase $ do
  let dv0    = V.fromList []
      dv     = V.fromList [ve 0 10 'a', ve 10 20 'b']
      dvBad  = V.fromList [ve 0 10 'a', ve 05 20 'b']
  assertBool "0 length 0" $ dvDur dv0 == 0
  assertBool "1 length 0" $ dvDur dv  == 30
