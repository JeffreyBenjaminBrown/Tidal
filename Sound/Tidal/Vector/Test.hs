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
  [ TestLabel "testVecEv" testVecEv
  ]

testVecEv = TestCase $ do
  let unsorted = V.fromList [ve 10  2 m, ve 10 1 m, ve 1  20 m]
      sorted =   V.fromList [ve 1  20 m, ve 10 1 m, ve 10  2 m]
      dvUnsorted = DurVec {_dvPeriod = 1, _dvPayload = unsorted}
      dvSorted   = DurVec {_dvPeriod = 1, _dvPayload =   sorted}
  assertBool "1" $ sortDurVec' dvUnsorted == dvSorted
