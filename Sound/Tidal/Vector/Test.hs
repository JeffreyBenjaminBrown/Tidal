module Sound.Tidal.Vector.Test where

import qualified Data.Vector as V
import Test.HUnit

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types

import Sound.Tidal.Vector.DurVec
import Sound.Tidal.Vector.VecEv


ve start duration payload =
  VecEv {veStart = start, veDuration = duration, vePayload = payload}
j = Just
n = Nothing
m = n :: Maybe Int

main = runTestTT $ TestList
  [ TestLabel "testSortDurVec" testSortDurVec
  , TestLabel "testVecEv" testVecEv
  , TestLabel "testArc" testArc
  ]

testArc = TestCase $ do
  let dv = durVec 5 [vecEv 0 2 'a', vecEv 1 2 'b', vecEv 2 2 'c']
  assertBool "1" $ arc (0,3) dv == [((1,2),'a'), ((2,3),'b')]

testVecEv = TestCase $ do
  let dv = durVec 5 [vecEv 0 2 'a', vecEv 1 2 'b', vecEv 2 2 'c']
  assertBool "0" $ end dv == 4
  assertBool "not finished" $ error "not finished"

testSortDurVec = TestCase $ do
  let unsorted = V.fromList [ve 10  2 m, ve 10 1 m, ve 1  20 m]
      sorted =   V.fromList [ve 1  20 m, ve 10 1 m, ve 10  2 m]
      dvUnsorted = DurVec {_dvPeriod = 1, _dvPayload = unsorted}
      dvSorted   = DurVec {_dvPeriod = 1, _dvPayload =   sorted}
  assertBool "1" $ sortDurVec' dvUnsorted == dvSorted
