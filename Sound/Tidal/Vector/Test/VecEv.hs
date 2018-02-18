module Sound.Tidal.Vector.Test.VecEv where

import Test.HUnit
import Sound.Tidal.Vector.Types
import Sound.Tidal.Vector.VecEv

testVecEv = [ TestLabel "testSupport" testSupport
            , TestLabel "testOverlapOfEv" testOverlapOfEv
            , TestLabel "testClip" testClip]

testSupport = TestCase $ do
  assertBool "0" $ support (vecEv 1 3 'a') == (1,4)

testOverlapOfEv = TestCase $ do
  assertBool "0" $ overlapOfEv (2,3) (vecEv 1 3 'a') == Just (2,3)
  assertBool "1" $ overlapOfEv (2,3) (vecEv 1 1 'a') == Nothing
  assertBool "1" $ overlapOfEv (2,3) (vecEv 2 0 'a') == Just (2,2)
  assertBool "1" $ overlapOfEv (2,3) (vecEv 3 0 'a') == Nothing

testClip = TestCase $ do
  assertBool "0" $ clip (2,3) (vecEv 1 3 'a') == vecEv 2 1 'a'
