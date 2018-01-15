{-# LANGUAGE FlexibleContexts  #-}

module Sound.Tidal.Epic.Test (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio ((%))
import Test.HUnit
import Text.Megaparsec

import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Abbreviations
import Sound.Tidal.Epic.CombineEpics
import Sound.Tidal.Epic.DirtNetwork
import Sound.Tidal.Epic.Parse.Eq
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Lexeme
import Sound.Tidal.Epic.Scale
import Sound.Tidal.Epic.Parse.Transform
import Sound.Tidal.Epic.Parse.Types
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
  , TestLabel "testApplyEpic2" testApplyEpic2
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
  , TestLabel "tDoubleThforationZeroBoundaries" tDoubleThforationZeroBoundaries
  , TestLabel "testApplyMetaEpic" testApplyMetaEpic
  , TestLabel "testScanAccumEpic" testScanAccumEpic
  , TestLabel "testScanLang" testScanLang
  , TestLabel "testLexemeToAccumEpic" testLexemeToAccumEpic
  , TestLabel "testLexeme" testLexeme
  , TestLabel "testPLang" testPLang
  , TestLabel "testPEpicOrOps" testPEpicOrOps
  , TestLabel "testPEpic" testPEpic
  , TestLabel "testSilence" testSilence
  , TestLabel "testParseScale" testParseScale
  , TestLabel "testBreathe" testBreathe
  , TestLabel "testBreathyConcatEpic" testBreathyConcatEpic
  , TestLabel "testBreathyConcatEpicIdea" testBreathyConcatEpicIdea
  , TestLabel "testSyParams" testSyParams
  ]

testSyParams = TestCase $ do
  let x = (loopa 1 $ M.singleton gain_p $ VF 1)
          +| (loopa 1 $ M.singleton speed_p $ VF 2)
      y = (loopa 1 $ M.singleton amp_p $ VF 1)
          +| (loopa 1 $ M.singleton qf_p $ VF 2)
  assertBool "1" $ syParams x == y

testBreathyConcatEpicIdea = TestCase $ do
  let x = dsh 1 +- loopa 1 'a'
  assertBool "1" $ arc (late 1 $ breathe 5 $ early 1 x) (0,7)
    == [((1,2),'a'),((6,7),'a')]
  let y = loopa 2 'a' +- dsh 1 -- imagine e +- y, where period e = 2
  assertBool "2" $ arc (late 2 $ breathe 5 y) (0,8)
    == [((2,4),'a'),((7,8),'a')]

testBreathyConcatEpic = TestCase $ do
  let ab = sparse 2 $ loopa 1 'a' +- loopa 1 'b'
      c =             loopa 2 'c' +- dsh 1
  assertBool "1" $ arc (eConcat ab c) (0,5)
    == [((0,2),'a'), ((2, 4),'c')]
  assertBool "2" $ arc (eConcat ab c) (5,10)
    == [((5,7),'b'), ((7,9),'c')]

testBreathe = TestCase $ do
  let x = loopa 1 'a' +- loopa 1 'b' +- dsh 1
      xb = breathe 5 x
  assertBool "no bug,  1" $ arc xb (1,9) /= []
  assertBool "no bug,  2" $ arc xb (2,9) /= []
  assertBool "bug,  3" $ arc xb (3,9) /= []
  assertBool "bug,  4" $ arc xb (4,9) /= []
  assertBool "no bug,  5" $ arc xb (5,9) /= []
  assertBool "no bug,  6" $ arc xb (6,9) /= []
  assertBool "bug, neg 1" $ arc xb (-1,9) /= []
  assertBool "bug, neg 2" $ arc xb (-2,9) /= []
  assertBool "not bug, neg 3" $ arc xb (-3,9) /= []
  assertBool "not bug, neg 4" $ arc xb (-4,9) /= []
  assertBool "not bug, neg 5" $ arc xb (-5,9) /= []
  assertBool "bug, neg 6" $ arc xb (-6,9) /= []

  assertBool "1"   $ breathAddGaps 5 2 (0,11) == [(0,2),(5,7),(10,11)]
  assertBool "1.1" $ breathAddGaps 5 2 (11,21) == [(11,12),(15,17),(20,21)]
  assertBool "1.2" $ breathAddGaps 5 2 (9,24) == [(10,12),(15,17),(20,22)]
  assertBool "2"   $ breathContract 5 2 (10,11) == (4,5)
  assertBool "2.1" $ breathContract 5 2 (11,12) == (5,6)
  assertBool "2.2" $ breathContract 5 2 (0,0) == (0,0)
  assertBool "2.3" $ breathContract 5 2 (15,15) == (6,6)
  assertBool "3" $ breathExpand 5 2 (4,5) == (10,11)
  assertBool "3.1" $ breathExpand 5 2 (5,6) == (11,12)
  let small = loopa 1 'a' +- loopa 1 'b'
      big = breathe 5 small
  assertBool "4" $ arc big (0,6) == [((0,1),'a'), ((1,2),'b'), ((5,6),'a')]
  let x = loopa 1 'a' +- loopa 1 'b' +- dsh 1
  assertBool "5" $
    arc          (breathe 5 $ early 1 x) (0,5) ==
    [((0 % 1,1 % 1),'b'),((2 % 1,3 % 1),'a')]
  assertBool "this is just like that, except plus a late" $
    arc (late 1 $ breathe 5 $ early 1 x) (0,10) /= []


-- The scale is only in effect for the first period.
testParseScale = TestCase $ do
  let scale = maj
      eScale = ps "maj"
      eDegs = pe "d0 +- d2 +- d4"
  assertBool "because <*> fails" $ (scale <$> eDegs) == (eScale <*> eDegs)

testPEpic = TestCase $ do
  let str = "s1.2"
      str2 = "[s1.2]"
      str2p5 = "[s1.2,,1d2]"
      str3 = "s1.2 +- d2"
      str4 = "s1.2 +- d2 +| d3"
      str5 = "s1.2 +- [d2 +| *2 d3]"
      sm = M.singleton speed_p $ VF 1.2
      dm2 = M.singleton deg_p $ VF 2
      dm3 = M.singleton deg_p $ VF 3
  assertBool "1" $ pe str == loopa 1 sm
  assertBool "2" $ pe str2 == loopa 1 sm
  assertBool "2p5" $ pe str2p5 == loopa 1 (M.union sm dm2)
  assertBool "3" $ pe str3 == (loopa 1 sm +- loopa 1 (M.union sm dm2))
  assertBool "4" $ pe str4 == (    (loopa 1 sm +- loopa 1 (M.union sm dm2))
                                   +| loopa 1  (M.union sm dm3)
                                 )
  assertBool "5" $ pe str5 == (    loopa 1 sm
                                   +- (    loopa 1 (M.union sm dm2)
                                        +| (fast 2 $ loopa 1 $ M.union sm dm3)
                                      ) )

testPEpicOrOps = TestCase $ do
  let str = "s1.2 +- 1d2 +| 1d3"
      sm = M.singleton speed_p $ VF 1.2
      dm2 = M.singleton deg_p $ VF 2
      dm3 = M.singleton deg_p $ VF 3
      -- The Eq instances for EpicWrap, UnaryWrap, BinaryWrap always fail.
      -- Therefore I instead Eq-test those wrappers' contents.
      Right [ EpicNotOp (EpicWrap e1)
            , BinaryOp (BinaryWrap o1)
            , EpicNotOp (EpicWrap e2)
            , BinaryOp (BinaryWrap o2)
            , EpicNotOp (EpicWrap e3)
            ] = parse (peEpicOrOps loopa) "" str
  assertBool "e1" $ e1 == loopa 1 sm
  assertBool "e2" $ e2 == loopa 1 (M.union sm dm2)
  assertBool "o1" $ o1 == eConcat
  assertBool "o2" $ o2 == stack
  assertBool "e3" $ e3 == loopa 1 (M.union sm dm3)

testPLang = TestCase $ do
  let str = "s1.2,,1d2 +| t2%3,,_ *2 +- t2%3"
      Right parsed = parse peLang "" str
      shouldBe = [ LangEpic ( AccumEpic Nothing
                              (M.singleton deg_p $ VF 2)
                              (M.singleton speed_p $ VF 1.2)
                              False )
                 , LangNonEpic $ NonEpicLexemeBinOp stack
                 , LangEpic ( AccumEpic (Just $ 2%3) M.empty M.empty True )
                 , LangNonEpic $ NonEpicLexemeUnOp $ fast 2
                 , LangNonEpic $ NonEpicLexemeBinOp eConcat
                 , LangEpic ( AccumEpic (Just $ 2%3) M.empty M.empty False )
                 ]
  assertBool "1" $ parsed == shouldBe

testLexeme = TestCase $ do
  let str = "s1.2,,1d2 *2 +| +- _,,t2%3 "
  assertBool "1" $ parse peLexemes "" str == Right
    [ LexemeEpics [ EpicPhonemeNewPersist $ M.singleton speed_p $ VF 1.2
                 , EpicPhonemeOnce $ M.singleton deg_p $ VF 2
                 ]
    , LexemeNonEpic (NonEpicLexemeUnOp $ fast 2)
    , LexemeNonEpic (NonEpicLexemeBinOp stack)
    , LexemeNonEpic (NonEpicLexemeBinOp eConcat)
    , LexemeEpics [ EpicPhonemeSilent
                 , EpicPhonemeFor $ 2%3
                 ]
    ]

testScanLang = TestCase $ do
  let (j,n,t,f) = (Just, Nothing,True,False)
      cdm = [ LangEpic (AccumEpic (j 2) n (j 3) f)
            , LangNonEpic (NonEpicLexemeUnOp (+1))
            , LangNonEpic NonEpicLexemeLeftBracket
            ] -- scanLang doesn't match brackets, just converts them
      [(EpicNotOp (EpicWrap ep)), UnaryOp (UnaryWrap g), LeftBracket]
        = scanLang loopa cdm
  assertBool "1" $ arc (g ep) (0,3) == [((0 % 1,2 % 1),4),((2 % 1,3 % 1),4)]

testScanAccumEpic = TestCase $ do
  let (cdm, j,n,t,f) = (AccumEpic, Just, Nothing, True, False)
      cdms1 = [cdm n n n f] :: [AccumEpic (Maybe Int)]
  assertBool "1" $ _scanAccumEpic cdms1 == map (uncurry Timed) [(1,n)]
  let cdms2 = [ cdm (j 2) n     (j 3) f
              , cdm n     n     n     f
              , cdm (j 1) (j 4) (j 5) f
              , cdm n     n     n     f
              , cdm n     n     n     t]
  assertBool "2" $ _scanAccumEpic cdms2 == map (uncurry Timed)
    [(2,j 3), (2,j 3), (1,j 4), (1,j 5), (1, n)]

testSilence = TestCase $ do
  assertBool "1" $ arc (pe0 "_+-s1") (0,2)
    == [((1,1),M.singleton speed_p $ VF 1)]

testApplyMetaEpic = TestCase $ do
  let me = cata 1 [id, fast 2]
      e  = cat0 1 [1, 2]
      e' = meta me e
  assertBool "1" $ arc e' (0,2) == [((  0,   0), 1)
                                    ,((  1,   1), 1)
                                    ,((1.5, 1.5), 2)]

testMergeEpics = TestCase $ do
  let f = speed $ cat0 (1%4) [2,1]
  assertBool "f1" $
    arc (speed (ever 2) &* f             ) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]
  assertBool "f2" $
    arc (f              &* speed (ever 2)) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]

testLexemeToAccumEpic = TestCase $ do
  let dur = 1
      soundMap = M.singleton sound_p $ VS "hatc"
      speedMap = M.singleton speed_p $ VF 2
      degMap = M.singleton deg_p $ VF 3
      parseBitSet = S.fromList [ EpicPhonemeFor dur
                               , EpicPhonemeNewPersist soundMap
                               , EpicPhonemeNewPersist degMap
                               , EpicPhonemeOnce speedMap ]
      seqBit = AccumEpic
               (Just dur) speedMap (M.union soundMap degMap) False
  assertBool "1" $ seqBit == lexemeToAccumEpic parseBitSet

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

tDoubleThforationZeroBoundaries = TestCase $ do
  assertBool "1" $ _doubleThforationZeroBoundaries [(0,0),(0,1)] [0,1]
    == [0,0,1]
  assertBool "2" $ _doubleThforationZeroBoundaries
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
      x1 = pure 10
      a = (4,6)
      ev = [(a,10)]
      f p = arc p a
  assertBool "functor 1" $ arc (fmap id x1) a == ev
  let p1 = fmap ((+2).(*3)) x1
      p2 = fmap (+2) . fmap (*3) $ x1
    in assertBool "functor 2" $ f p1 == f p2
  assertBool "applic 1" $ arc (f1 <*> x1) a == ev
  assertBool "applic 2" $ f (f2 <*> x1) == f (pure $ (+2) 10)
  -- ambition ? could test the other 2 applicative laws, but don't know why

testStack = TestCase $ do
  let p1 =          for 1 'a'
      p2 = late 1 $ for 1 'b'
  assertBool "1" $ arc (stack p1 p2) (0,1) == [((0,1),'a')]

testConcatEpic = TestCase $ do
  let p1 = loope 2 $ for 1 'a'
      p2 = loope 3 $ for 1 'b'
      p = eConcat p1 p2
  assertBool "1" $ period p == (Just 5)
  assertBool "2" $ arc p (0,7) ==  [((0,1),'a')
                                    ,((2,3),'b')
                                    ,((5,6),'a')]

testEpicPatternIsh = TestCase $ do
  let p = for 1 () :: Epic ()
  assertBool "1" $ arc (early (1/2) p) (-1,1) == [((-1/2, 1/2), ())]
  assertBool "2" $ arc (late  (1/2) p) (-1,1) == [((1/2 , 1  ), ())]
  assertBool "3" $ arc (slow 2     p) (-1,3) == [((  0 , 2  ), ())]
  assertBool "4" $ arc (fast 2     p) (-1,3) == [((  0 , 1/2), ())]
  assertBool "5" $ arc (rev        p) (-2,1) == [(( -1 , 0  ), ())]
  let q = stack (late 1 $ fmap (const 1) p)
                 (late 3 $ fmap (const 2) p)
  assertBool "6" $ arc (rev q) (-5,1) == [ (( -4 , -3  ), 2)
                                           , (( -2 , -1  ), 1) ]

testApplyEpic = TestCase $ do
  let f1 =          for 1 (+1)
      f2 = late 2 $ for 2 (+2)
      f3 = late 4 $ for 2 (+3)
      g1 = Epic Nothing $ \a -> arc f1 a ++ arc f2 a ++ arc f3 a
      x = loope 2 $ for 1 (1 :: Int)
  assertBool "1" $ arc (f1 <*> x) (0,4) == [((0,1),2)]
  assertBool "tricky!" $ arc (g1 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),4) ]
  -- An earlier version of takeOverlappingEvs caused the event at time 4
  -- to undergo both (+2) (with duration 0) and (+3) (with duration 1).
  let g2 = loope 4 g1
  assertBool "3" $ arc (g2 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),2),  ((6,7),3)]
  assertBool "4" $ period g2 == Just 4
  assertBool "5" $ period (g2 <*> x) == Just 4
  let f = stack (for 1 (+1)) (late (1/2) $ for 1 (+2))
      x =         for 1 1
  assertBool "overlapping functions induce concurrency"
    $ arc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f =         for 1 (+1)
      x = stack (for 1 1  ) (late (1/2) $ for 1 2)
  assertBool "overlapping objects are concurrency"
    $ arc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f = stack (for 1 (+1)) (late (1/2) $ for 1 (+2))
      x = stack (for 1 1  ) (late (1/2) $ for 1 2)
  assertBool "concurrent objects and functions multiply like the list monad"
    $ arc (f <*> x) (0,1)
    == [((0,1), 2),  ((1/2,1),3),  ((1/2,1), 3),  ((1/2,1), 4)]

testApplyEpic2 = TestCase $ do
  let e1 = loopa 1 (+1) <*> (loopa 1 1 +- loopa 1 2)
      e2 =                   loopa 1 2 +- loopa 1 3
  assertBool "works on unit interval" $ arc e1 (0,1) == arc e2 (0,1)
  assertBool "<*> fails beyond (0,1)" $ arc e1 (0,2) == arc e2 (0,2)

testWindow = TestCase $ do
  let ep = window (2,5) $ loope 2 $ for 1 ()
  assertBool "1" $ arc ep (2,5) == [((2,3),())
                                    ,((4,5),())]

testTakeOverlappingEvents = TestCase $ do
  assertBool "1" $ takeOverlappingEvs (0,1) [((0,1),())]
    == [((0,1),())]
  assertBool "2" $ takeOverlappingEvs (0,2) [((0,1),()), ((1,2),())]
    == [ ((0,1),()), ((1,2),()) ]
  assertBool "3" $ takeOverlappingEvs (0,2) [((-1,1),()), ((1.5,2.5),())]
    == [ ((0,1),()), ((1.5,2),()) ]

testEDur = TestCase $ do
  let p1 = for 1 ()
      events1 = arc p1 (0,2)
      a1 = head events1
  assertBool "interval" $ fst a1 == (0,1)
  assertBool "exactly one event" $ length events1 == 1
  assertBool "no overlap, early" $ null $ arc p1 (-3,-2)
  assertBool "no overlap, late" $ null $ arc p1 (2,3)
  let events2 = arc p1 (1/2,3/2)
      a2 = head events2
  assertBool "interval 2" $ fst a2 == (1/2,1)
  assertBool "exaclty one event, again" $ length events2 == 1

testEInstant = TestCase $ do
  let Epic Nothing f = instant ()
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
  let p = loope 2 $ instant ()
  assertBool "1" $ arc p (0,4) == [((0,0),()), ((2,2),())]

testLcmRatios = TestCase $ do
  assertBool "1" $ lcmRatios (1%3) (1%6) == (1%3)
  assertBool "2" $ lcmRatios (1%3) (1%2) == 1
