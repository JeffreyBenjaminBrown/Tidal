{-# LANGUAGE FlexibleContexts  #-}

module Sound.Tidal.Epic.Test (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio ((%))
import Test.HUnit
import Text.Megaparsec

import Sound.Tidal.Epic.Types.Reimports
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.DirtNetwork
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Cmd
import Sound.Tidal.Epic.Scales
import Sound.Tidal.Epic.Parse.SeqCommand
import Sound.Tidal.Epic.Parse.SeqCommand2Stage
import Sound.Tidal.Epic.Sounds
import Sound.Tidal.Epic.Transform
import Sound.Tidal.Epic.Util

main = runTestTT $ TestList
  [ TestLabel "testEDur" testEDur
  , TestLabel "testMergeNumParamsWith" testMergeNumParamsWith
  , TestLabel "testMergeEvents" testMergeEvents
  , TestLabel "testMergeEpics" testMergeEpics
  , TestLabel "testEInstant" testEInstant
  , TestLabel "testTakeOverlappingEvents" testTakeOverlappingEvents
  , TestLabel "testApplyEpic" testApplyEpic
  , TestLabel "testEpicPatternIsh" testEpicPatternIsh
  , TestLabel "testERepeat" testERepeat
  , TestLabel "testLcmRatios" testLcmRatios
  , TestLabel "testWindow" testWindow
  , TestLabel "testConcatEpic" testConcatEpic
  , TestLabel "testStack" testStack
  , TestLabel "testLaws" testLaws
  , TestLabel "testPartitionArcAtBoundaries" testPartitionArcAtBoundaries
  , TestLabel "testPartitionAndGroupEvents" testPartitionAndGroupEvents
  , TestLabel "testOverlap" testOverlap
  , TestLabel "tDoubleTheDurationZeroBoundaries" tDoubleTheDurationZeroBoundaries
  , TestLabel "testToCmdBlock" testToCmdBlock
  , TestLabel "testBlocksToEpic" testBlocksToEpic
  , TestLabel "testParseCmd" testParseCmd
  , TestLabel "testParamMapSeq" testParamMapSeq
  , TestLabel "testSyFreq" testSyFreq
  , TestLabel "testApplyMetaEpic" testApplyMetaEpic
  , TestLabel "testSilence" testSilence
  , TestLabel "testCxDurScanAccum" testCxDurScanAccum
  ]

testCxDurScanAccum = TestCase $ do
  let (cdm, j,n,t,f) = (CxDurMonoid, Just, Nothing, True, False)
      cdms1 = [cdm n n n f] :: [CxDurMonoid (Maybe Int)] 
  assertBool "1" $ _cxDurScanAccum cdms1 == map toDurMonoid [(1,n)]
  let cdms2 = [ cdm (j 2) n     (j 3) f
              , cdm n     n     n     f
              , cdm (j 1) (j 4) (j 5) f
              , cdm n     n     n     f
              , cdm n     n     n     t]
  assertBool "2" $ _cxDurScanAccum cdms2 == map toDurMonoid
    [(2,j 3), (2,j 3), (1,j 4), (1,j 5), (1, n)]

testSilence = TestCase $ do
  assertBool "1" $ eArc (p0 "_,,s1") (0,2)
    == [((1,1),M.singleton speed_p $ VF 1)]

testApplyMetaEpic = TestCase $ do
  let me = cata 1 [id, fast 2]
      e  = cat0 1 [1, 2]
      e' = applyMetaEpic me e
  assertBool "1" $ eArc e' (0,2) == [((  0,   0), 1)
                                    ,((  1,   1), 1)
                                    ,((1.5, 1.5), 2)]

testMergeEpics = TestCase $ do
  let f = speed $ cat0 (1%4) [2,1]
  assertBool "f1" $
    eArc (speed (ever 2) &* f             ) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]
  assertBool "f2" $
    eArc (f              &* speed (ever 2)) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]

testSyFreq = TestCase $ do
  let f = _syFreq $     M.fromList [(speed_p,VF 3),(sound_p,VS "s")]
  assertBool "1" $ f == M.fromList [(qf_p,VF 3),   (sound_p,VS "s")]

testParamMapSeq = TestCase $ do
  let Right x = parse paramMapSeq0 "" "t1 g2 ,, t2 s3"
  assertBool "1" $ eArc x (0,4) == [
    ((0 % 1,0 % 1),M.fromList [(gain_p,VF 2.0)])
    ,((1 % 1,1 % 1),M.fromList [(speed_p,VF 3.0),(gain_p,VF 2.0)])
    ,((3 % 1,3 % 1),M.fromList [(gain_p,VF 2.0)])]

testParseCmd = TestCase $ do
  assertBool "1" $ parse cmd "" "t1" == Right (CmdDur 1)
  assertBool "2" $ parse cmd "" "t1%2" == Right (CmdDur $ 1%2)
  assertBool "3" $ parse cmd "" "g1.2"
    == Right (CmdParamPersist $ M.singleton gain_p $ VF 1.2)
  assertBool "4" $ parse cmd "" "1_hatc"
    == Right (CmdParamOnce $ M.singleton sound_p $ VS "hatc")

testBlocksToEpic = TestCase $ do
  let aCmdBlockList = [ CmdBlock (Just 1) False
                        (M.fromList [(sound_p,VS "bow"), (speed_p,VF 2)])
                        (M.fromList [(deg_p,VF 3.5)])
                      , CmdBlock (Just 2) False (M.singleton speed_p $ VF 4)
                        M.empty
                      ] :: [CmdBlock]
  assertBool "1" $ eArc (blocksToEpic0 aCmdBlockList) (0,3) ==
    [ ((0 % 1,0 % 1),M.fromList [(sound_p,VS "bow"),(deg_p,VF 3.5)
                                ,(speed_p,VF 2.0)])
    , ((1 % 1,1 % 1),M.fromList [(sound_p,VS "bow"),(speed_p,VF 4.0)]) ]

testToCmdBlock = TestCase $ do
  let dur = 1
      soundMap = M.singleton sound_p $ VS "hatc"
      speedMap = M.singleton speed_p $ VF 2
      degMap = M.singleton deg_p $ VF 3
      parseBitSet = S.fromList [ CmdDur dur
                               , CmdParamPersist soundMap
                               , CmdParamPersist degMap
                               , CmdParamOnce speedMap ]
      seqBit = CmdBlock (Just dur) False (M.union soundMap degMap) speedMap
  assertBool "1" $ seqBit == toCmdBlock parseBitSet

testMergeEvents = TestCase $ do
  let aEvs = [ ((0 % 1,0 % 1),M.fromList [(speed_p,VF 2)])
             , ((1 % 2,1 % 2),M.fromList [(speed_p,VF 1)]) ]
      bEvs = [ ((0 % 1,1 % 1),M.fromList [(speed_p,VF 2)]) ]
  assertBool "0" $ mergeEvents (*) (*) aEvs bEvs ==
    [ ((0 % 1,0 % 1),M.fromList [(speed_p,VF 4)])
    , ((1 % 2,1 % 2),M.fromList [(speed_p,VF 2)]) ]
  let a = M.fromList [(gain_p, VF 2), ( coarse_p,VF 2)]
      b = M.fromList [(gain_p, VF 3), ( crush_p, VF 3)]
      c = M.fromList [( cut_p, VF 4), ( crush_p, VF 4)]
  assertBool "1" $ mergeEvents (*) (*) [((0,1),a), ((0,2),b)]  [((1,1), c)]
    == [((1,1), M.fromList[(crush_p,VF 12),(cut_p,VF 4),(gain_p, VF 3)])]

tDoubleTheDurationZeroBoundaries = TestCase $ do
  assertBool "1" $ _doubleTheDurationZeroBoundaries [(0,0),(0,1)] [0,1]
    == [0,0,1]
  assertBool "2" $ _doubleTheDurationZeroBoundaries
    [(0,0),(0,1),(0,2),(1,1),(1,2),(2,3)] [0..3] == [0,0,1,1,2,3]

testMergeNumParamsWith = TestCase $ do
  assertBool "1" $ mergeNumParamsWith (*) (*)
    (M.fromList   [(gain_p, VF 1), (crush_p, VF 2)])
    (M.fromList   [(gain_p, VF 2), (crush_p, VF 3)])
    == M.fromList [(gain_p, VF 2), (crush_p, VF 6)]

testPartitionAndGroupEvents = TestCase $ do
  assertBool "1" $
    S.fromList ( partitionAndGroupEvents
                   [((1,2),'c'), ((0,1),'a'), ((0,2),'b')] )
    == S.fromList [((0,1),'a'), ((0,1),'b'), ((1,2),'b'), ((1,2),'c')]
  assertBool "1" $
    S.fromList ( partitionAndGroupEvents
                   [((1,2),'c'), ((0,1),'a'), ((0,2),'b'), ((1,1),'z')] )
    == S.fromList [ ((0,1),'a'), ((0,1),'b')
                    , ((1,1),'z'), ((1,1),'c'), ((1,1),'b')
                    , ((1,2),'b'), ((1,2),'c')]

testPartitionArcAtBoundaries = TestCase $ do
  assertBool "1" $ partitionArcAtTimes [0,1,2,3,4] (1,3)
    == [(1,2),(2,3)]

testLaws = TestCase $ do
  let f1 = pure id :: Epic (a -> a)
      f2 = pure (+2) :: Epic (Float -> Float)
      f3 = pure (*3) :: Epic (Float -> Float)
      x1 = pure 10
      a = (4,6)
      ev = [(a,10)]
      f p = eArc p a
  assertBool "functor 1" $ eArc (fmap id x1) a == ev
  let p1 = fmap ((+2).(*3)) x1
      p2 = fmap (+2) . fmap (*3) $ x1
    in assertBool "functor 2" $ f p1 == f p2
  assertBool "applic 1" $ eArc (f1 <*> x1) a == ev
  assertBool "applic 2" $ f (f2 <*> x1) == f (pure $ (+2) 10)
  -- ambition ? could test the other 2 applicative laws, but don't know why

testStack = TestCase $ do
  let p1 =          eDur 1 'a'
      p2 = late 1 $ eDur 1 'b'
  assertBool "1" $ eArc (eStack p1 p2) (0,1) == [((0,1),'a')]

testConcatEpic = TestCase $ do
  let p1 = eRepeat 2 $ eDur 1 'a'
      p2 = eRepeat 3 $ eDur 1 'b'
      p = concatEpic p1 p2
  assertBool "1" $ period p == (Just 5)
  assertBool "2" $ eArc p (0,7) ==  [((0,1),'a')
                                    ,((2,3),'b')
                                    ,((5,6),'a')]

testEpicPatternIsh = TestCase $ do
  let p = eDur 1 () :: Epic ()
  assertBool "1" $ eArc (early (1/2) p) (-1,1) == [((-1/2, 1/2), ())]
  assertBool "2" $ eArc (late  (1/2) p) (-1,1) == [((1/2 , 1  ), ())]
  assertBool "3" $ eArc (eSlow 2     p) (-1,3) == [((  0 , 2  ), ())]
  assertBool "4" $ eArc (eFast 2     p) (-1,3) == [((  0 , 1/2), ())]
  assertBool "5" $ eArc (eRev        p) (-2,1) == [(( -1 , 0  ), ())]
  let q = eStack (late 1 $ fmap (const 1) p)
                 (late 3 $ fmap (const 2) p)
  assertBool "6" $ eArc (eRev q) (-5,1) == [ (( -4 , -3  ), 2)
                                           , (( -2 , -1  ), 1) ]

testApplyEpic = TestCase $ do
  let f1 =          eDur 1 (+1)
      f2 = late 2 $ eDur 2 (+2)
      f3 = late 4 $ eDur 2 (+3)
      g1 = Epic Nothing $ \arc -> eArc f1 arc ++ eArc f2 arc ++ eArc f3 arc
      x = eRepeat 2 $ eDur 1 (1 :: Int)
  assertBool "1" $ eArc (f1 <*> x) (0,4) == [((0,1),2)]
  assertBool "tricky!" $ eArc (g1 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),4) ]
  -- An earlier version of takeOverlappingEvs caused the event at time 4
  -- to undergo both (+2) (with duration 0) and (+3) (with duration 1).
  let g2 = eRepeat 4 g1
  assertBool "3" $ eArc (g2 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),2),  ((6,7),3)]
  assertBool "4" $ period g2 == Just 4
  assertBool "5" $ period (g2 <*> x) == Just 4
  let f = eStack (eDur 1 (+1)) (late (1/2) $ eDur 1 (+2))
      x =         eDur 1 1
  assertBool "overlapping functions induce concurrency"
    $ eArc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f =         eDur 1 (+1)
      x = eStack (eDur 1 1  ) (late (1/2) $ eDur 1 2)
  assertBool "overlapping objects are concurrency"
    $ eArc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f = eStack (eDur 1 (+1)) (late (1/2) $ eDur 1 (+2))
      x = eStack (eDur 1 1  ) (late (1/2) $ eDur 1 2)
  assertBool "concurrent objects and functions multiply like the list monad"
    $ eArc (f <*> x) (0,1)
    == [((0,1), 2),  ((1/2,1),3),  ((1/2,1), 3),  ((1/2,1), 4)]

testWindow = TestCase $ do
  let ep = window (2,5) $ eRepeat 2 $ eDur 1 ()
  assertBool "1" $ eArc ep (2,5) == [((2,3),())
                                    ,((4,5),())]

testTakeOverlappingEvents = TestCase $ do
  assertBool "1" $ takeOverlappingEvs (0,1) [((0,1),())]
    == [((0,1),())]
  assertBool "2" $ takeOverlappingEvs (0,2) [((0,1),()), ((1,2),())]
    == [ ((0,1),()), ((1,2),()) ]
  assertBool "3" $ takeOverlappingEvs (0,2) [((-1,1),()), ((1.5,2.5),())]
    == [ ((0,1),()), ((1.5,2),()) ]

testEDur = TestCase $ do
  let p1 = eDur 1 ()
      events1 = eArc p1 (0,2)
      a1 = head events1
  assertBool "interval" $ fst a1 == (0,1)
  assertBool "exactly one event" $ length events1 == 1
  assertBool "no overlap, early" $ null $ eArc p1 (-3,-2)
  assertBool "no overlap, late" $ null $ eArc p1 (2,3)
  let events2 = eArc p1 (1/2,3/2)
      a2 = head events2
  assertBool "interval 2" $ fst a2 == (1/2,1)
  assertBool "exaclty one event, again" $ length events2 == 1

testEInstant = TestCase $ do
  let Epic Nothing f = eInstant ()
  assertBool "1" $ f (-2,-1) == []
  assertBool "2" $ f (1,2) == []
  assertBool "3" $ f (-0.5,0.5) == [ ((0,0), ()) ]

testOverlap = TestCase $ do
  assertBool "1" $ overlap (0,0) (0,1) == Just (0,0)
  assertBool "1" $ overlap (0,1) (0,0) == Just (0,0)
  assertBool "1" $ overlap (1,1) (0,1) == Nothing
  assertBool "1" $ overlap (0,1) (1,1) == Nothing
  assertBool "1" $ overlap (0,1) (1,2) == Nothing
  assertBool "1" $ overlap (1,2) (0,1) == Nothing
  assertBool "1" $ overlap (0,2) (1,3) == Just (1,2)

testERepeat = TestCase $ do
  let p = eRepeat 2 $ eInstant ()
  assertBool "1" $ eArc p (0,4) == [((0,0),()), ((2,2),())]

testLcmRatios = TestCase $ do
  assertBool "1" $ lcmRatios (1%3) (1%6) == (1%3)
  assertBool "2" $ lcmRatios (1%3) (1%2) == 1
