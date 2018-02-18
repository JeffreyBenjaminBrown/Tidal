module Sound.Tidal.Vector.Test.DurVec where

import qualified Data.Vector as V
import Test.HUnit

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types

import Sound.Tidal.Vector.DurVec
import Sound.Tidal.Vector.VecEv


testDurVec = [ TestLabel "testEnd" testEnd
             , TestLabel "testSortDurVec" testSortDurVec
             , TestLabel "testArc" testArc
             ]

j = Just
n = Nothing
m = n :: Maybe Int

testEnd = TestCase $ do
  let dv = durVec 5 [vecEv 0 2 'a', vecEv 1 2 'b', vecEv 2 2 'c']
  assertBool "0" $ end dv == 4

testSortDurVec = TestCase $ do
  let unsorted = V.fromList [vecEv 10  2 m, vecEv 10 1 m, vecEv 1  20 m]
      sorted =   V.fromList [vecEv 1  20 m, vecEv 10 1 m, vecEv 10  2 m]
      dvUnsorted = DurVec {_dvPeriod = 1, _dvPayload = unsorted}
      dvSorted   = DurVec {_dvPeriod = 1, _dvPayload =   sorted}
  assertBool "1" $ sortDurVec' dvUnsorted == dvSorted

testArc = TestCase $ do
  let dv = durVec 5 [vecEv 0 2 'a', vecEv 1 2 'b', vecEv 2 2 'c']
  assertBool "1" $ arc (0,3) dv == [((1,2),'a'), ((2,3),'b')]
