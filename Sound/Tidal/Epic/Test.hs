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
import Sound.Tidal.Epic.Harmony
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Params
import Sound.Tidal.Epic.Parse.Lexeme
import Sound.Tidal.Epic.Scale
import Sound.Tidal.Epic.Parse.Convert
import Sound.Tidal.Epic.Parse.Types
import Sound.Tidal.Epic.Parse.Phoneme.Transform
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
  , TestLabel "testBoundaries" testBoundaries
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
  , TestLabel "testParseTransform" testParseTransform
  , TestLabel "testSpace" testSpace
  , TestLabel "testBreathyConcatEpic" testBreathyConcatEpic
  , TestLabel "testBreathyConcatEpicIdea" testBreathyConcatEpicIdea
  , TestLabel "testSyParams" testSyParams
  , TestLabel "testWarp" testWarp
  , TestLabel "testRemap" testRemap
  , TestLabel "testPartialScore" testPartialScore
  , TestLabel "testDj" testDj
  ]

testDj = TestCase $ do
  let a = pe0 "_bd _sn"
      b = pe0 "_hc"
      theMap = M.fromList [("a",a),("b",b)]
      theEpic = dj (pdj "id,@a | ea1%2,@b") theMap
  assertBool "payload" $ _arc theEpic (0,2)
    == [ ((0,0),     M.singleton sound_p $ VS "bd")
       , ((1/2,1/2), M.singleton sound_p $ VS "hc")
       , ((1,1),     M.singleton sound_p $ VS "sn")
       , ((3/2,3/2), M.singleton sound_p $ VS "hc") ]
  assertBool "duration" $ Just 60 ==
    (_period $ dj (pdj "/12 id") $ M.fromList [("a",dsh 15),("b",dsh 20)])

testPartialScore = TestCase $ do
  let h = Harmony { baseScale = maj
                  , root = 1
                  , scaleSize = 7
                  , chord = [0,2,4] }
      rules = [InScale 1, InChord 10, DegDiff 100, Unique 1000]
      original = 3
      others = [0,4]
      score' = score h others original rules :: Degree -> Score
  assertBool "3"   $ score' 3 == 10
  assertBool "2"   $ score' 2 == 100
  assertBool "4"   $ score' 4 == 1100
  assertBool "3.5" $ score' 3.5 == 61 -- 1 + 10 + (1/2)*100

testRemap = TestCase $ do
  let theRemap = M.fromList [("a",deg_p)]
      abstract = M.fromList [("a",1)]
  assertBool "1" $ remapMd theRemap abstract == M.fromList [(deg_p,VF 1)]
  let ea = pm0 "a0 a2" -- Epic (Map String Double) -> ParamEpic
  assertBool "2" $ (remapMd theRemap <$> ea) == pe0 "d0 d2"
  let ed = pd0 "0 2" -- Epic Double -> ParamEpic
  assertBool "3" $ (remapPd deg_p <$> ed) == pe0 "d0 d2"

testWarp = TestCase $ do
  let f = warpTime 0 0 0 -- strength 0 + laziness => no division by 0
  assertBool "1" $ f 0 == 0
  assertBool "1" $ f 0.1 == 0.1
  assertBool "1" $ f (-0.1) == -0.1
  let g = warpTime 0.01 0.03 1
  assertBool "2" $ g 0 == 0
  assertBool "2" $ g 0.1 > 0.1
  assertBool "2" $ g 0.5 == 0.5
  assertBool "2" $ g 0.6 < 0.6
  let f = fast 4 $ cata 1 [1..4]
      wf = warp 0.001 0.30 1 f
      [   ((a,b),_)
        , ((a1,b1),_)
        , ((a2,b2),_)
        , ((a3,b3),_) ] = _arc wf (0,1)
  assertBool "3" $ b == a1 && b1 == a2 && b2 == a3
  assertBool "3" $ a == 0 && a1 == 3/10 && a2 == 1/2 && a3 == 7/10
  let f = fast 4 $ cata 2 [1..4]
      wf = warp 0.001 0.30 2 f
      [   ((a,b),_)
        , ((a1,b1),_)
        , ((a2,b2),_)
        , ((a3,b3),_) ] = _arc wf (0,2)
  assertBool "4" $ b == a1 && b1 == a2 && b2 == a3
  assertBool "4" $ a == 0 && a1 == 3/5 && a2 == 1 && a3 == 7/5 && b3 == 2
  return ()

testSyParams = TestCase $ do
  let x = (loopa 1 $ M.singleton gain_p $ VF 1)
          +| (loopa 1 $ M.singleton speed_p $ VF 2)
      y = (loopa 1 $ M.singleton qa_p $ VF 1)
          +| (loopa 1 $ M.singleton qf_p $ VF 2)
  assertBool "1" $ (syParams <$< x) == y

testBreathyConcatEpicIdea = TestCase $ do
  let x = dsh 1 +- loopa 1 'a'
  assertBool "1" $ _arc (late 1 $ space 5 $ early 1 x) (0,7)
    == [((1,2),'a'),((6,7),'a')]
  let y = loopa 2 'a' +- dsh 1 -- imagine e +- y, where period e = 2
  assertBool "2" $ _arc (late 2 $ space 5 y) (0,8)
    == [((2,4),'a'),((7,8),'a')]

testBreathyConcatEpic = TestCase $ do
  let ab = sparse 2 $ loopa 1 'a' +- loopa 1 'b'
      c =             loopa 2 'c' +- dsh 1
  assertBool "1" $ _arc (append ab c) (0,5)
    == [((0,2),'a'), ((2, 4),'c')]
  assertBool "2" $ _arc (append ab c) (5,10)
    == [((5,7),'b'), ((7,9),'c')]

testSpace = TestCase $ do
  let x = loopa 1 'a' +- loopa 1 'b' +- dsh 1
      xb = space 5 x
  assertBool "no bug,  1" $ _arc xb (1,9) /= []
  assertBool "no bug,  2" $ _arc xb (2,9) /= []
  assertBool "bug,  3" $ _arc xb (3,9) /= []
  assertBool "bug,  4" $ _arc xb (4,9) /= []
  assertBool "no bug,  5" $ _arc xb (5,9) /= []
  assertBool "no bug,  6" $ _arc xb (6,9) /= []
  assertBool "bug, neg 1" $ _arc xb (-1,9) /= []
  assertBool "bug, neg 2" $ _arc xb (-2,9) /= []
  assertBool "not bug, neg 3" $ _arc xb (-3,9) /= []
  assertBool "not bug, neg 4" $ _arc xb (-4,9) /= []
  assertBool "not bug, neg 5" $ _arc xb (-5,9) /= []
  assertBool "bug, neg 6" $ _arc xb (-6,9) /= []

  assertBool "1"   $ addGapsForSpace 5 2 (0,11) == [(0,2),(5,7),(10,11)]
  assertBool "1.1" $ addGapsForSpace 5 2 (11,21) == [(11,12),(15,17),(20,21)]
  assertBool "1.2" $ addGapsForSpace 5 2 (9,24) == [(10,12),(15,17),(20,22)]
  assertBool "2"   $ contractForSpace 5 2 (10,11) == (4,5)
  assertBool "2.1" $ contractForSpace 5 2 (11,12) == (5,6)
  assertBool "2.2" $ contractForSpace 5 2 (0,0) == (0,0)
  assertBool "2.3" $ contractForSpace 5 2 (15,15) == (6,6)
  assertBool "3"   $ expandForSpace 5 2 (4,5) == (10,11)
  assertBool "3.1" $ expandForSpace 5 2 (5,6) == (11,12)
  let small = loopa 1 'a' +- loopa 1 'b'
      big = space 5 small
  assertBool "4" $ _arc big (0,6) == [((0,1),'a'), ((1,2),'b'), ((5,6),'a')]
  let x = loopa 1 'a' +- loopa 1 'b' +- dsh 1
  assertBool "5" $
    _arc          (space 5 $ early 1 x) (0,5) ==
    [((0 % 1,1 % 1),'b'),((2 % 1,3 % 1),'a')]
  assertBool "this is just like that, except plus a late" $
    _arc (late 1 $ space 5 $ early 1 x) (0,10) /= []

testParseTransform = TestCase $ do
  let Right tr = parse pParamMult "" "(g0.7,s3)"
      ep = pe "_sn"
  assertBool "1" $ _arc (tr ep) (0,1)
    == [((0,1), M.fromList [ (gain_p, VF 0.7)
                           , (sound_p, VS "sn")
                           , (speed_p, VF 3.0)])]

-- The scale is only in effect for the first period.
testParseScale = TestCase $ do
  let scale = maj
      eScale = ps "maj"
      eDegs = pe "d0 - d2 - d4"
  assertBool "because <*> fails" $ (scale <$> eDegs) == (eScale <*> eDegs)

testPEpic = TestCase $ do
  let str = "s1.2"
      str2 = "[s1.2]"
      str2p5 = "[s1.2,,1d2]"
      str3 = "s1.2 - d2"
      str4 = "s1.2 - d2 | d3"
      str5 = "s1.2 - [d2 | *2 d3]"
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
  let str = "s1.2 - 1d2 | 1d3"
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
  assertBool "o1" $ o1 == append
  assertBool "o2" $ o2 == stack
  assertBool "e3" $ e3 == loopa 1 (M.union sm dm3)

testPLang = TestCase $ do
  let str = "s1.2,,1d2 | t2%3,,_ *2 - t2%3"
      Right parsed = parse peLang "" str
      shouldBe = [ LangEpic ( AccumEpic Nothing
                              (M.singleton deg_p $ VF 2)
                              (M.singleton speed_p $ VF 1.2)
                              False )
                 , LangNonEpic $ NonEpicLexemeBinOp stack
                 , LangEpic ( AccumEpic (Just $ 2%3) M.empty M.empty True )
                 , LangNonEpic $ NonEpicLexemeUnOp $ fast 2
                 , LangNonEpic $ NonEpicLexemeBinOp append
                 , LangEpic ( AccumEpic (Just $ 2%3) M.empty M.empty False )
                 ]
  assertBool "1" $ parsed == shouldBe

testLexeme = TestCase $ do
  let str = "s1.2,,1d2 *2 | - _,,t2%3 "
  assertBool "1" $ parse peLexemes "" str == Right
    [ LexemeEpic [ EpicPhonemeNewPersist $ M.singleton speed_p $ VF 1.2
                 , EpicPhonemeOnce $ M.singleton deg_p $ VF 2
                 ]
    , LexemeNonEpic (NonEpicLexemeUnOp $ fast 2)
    , LexemeNonEpic (NonEpicLexemeBinOp stack)
    , LexemeNonEpic (NonEpicLexemeBinOp append)
    , LexemeEpic [ EpicPhonemeSilent
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
  assertBool "1" $ _arc (g ep) (0,3) == [((0 % 1,2 % 1),4),((2 % 1,3 % 1),4)]

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
  assertBool "1" $ _arc (pe0 "_ - s1") (0,2)
    == [((1,1),M.singleton speed_p $ VF 1)]

testApplyMetaEpic = TestCase $ do
  let me = cata 1 [id, fast 2]
      e  = cat0 1 [1, 2]
      e' = meta me e
  assertBool "1" $ _arc e' (0,2) == [((  0,   0), 1)
                                    ,((  1,   1), 1)
                                    ,((1.5, 1.5), 2)]

testMergeEpics = TestCase $ do
  let f = speed $ cat0 (1%4) [2,1]
  assertBool "f1" $
    _arc (speed (ever 2) &* f             ) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]
  assertBool "f2" $
    _arc (f              &* speed (ever 2)) (0,1%4) ==
    [((0,0), M.singleton speed_p $ VF 4)]

testLexemeToAccumEpic = TestCase $ do
  let dur = 1
      soundMap = M.singleton sound_p $ VS "hatc"
      speedMap = M.singleton speed_p $ VF 2
      degMap = M.singleton deg_p $ VF 3
      parseBitSet = [ EpicPhonemeFor dur
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

testBoundaries = TestCase $ do
  assertBool "1" $ boundaries [(0,0), (1,5), (1,3), (3,5)] == [0,0,1,3,5]

testLaws = TestCase $ do
  let f1 = pure id :: Epic (a -> a)
      f2 = pure (+2) :: Epic (Float -> Float)
      x1 = pure 10
      a = (4,6)
      ev = [(a,10)]
      f p = _arc p a
  assertBool "functor 1" $ _arc (fmap id x1) a == ev
  let p1 = fmap ((+2).(*3)) x1
      p2 = fmap (+2) . fmap (*3) $ x1
    in assertBool "functor 2" $ f p1 == f p2
  assertBool "applic 1" $ _arc (f1 <*> x1) a == ev
  assertBool "applic 2" $ f (f2 <*> x1) == f (pure $ (+2) 10)
  -- ambition ? could test the other 2 applicative laws, but don't know why

testStack = TestCase $ do
  let p1 =          for 1 'a'
      p2 = late 1 $ for 1 'b'
  assertBool "1" $ _arc (stack p1 p2) (0,1) == [((0,1),'a')]

testConcatEpic = TestCase $ do
  let p1 = loope 2 $ for 1 'a'
      p2 = loope 3 $ for 1 'b'
      p = append p1 p2
  assertBool "1" $ _period p == (Just 5)
  assertBool "2" $ _arc p (0,7) ==  [((0,1),'a')
                                    ,((2,3),'b')
                                    ,((5,6),'a')]

testEpicPatternIsh = TestCase $ do
  let p = for 1 () :: Epic ()
  assertBool "1" $ _arc (early (1/2) p) (-1,1) == [((-1/2, 1/2), ())]
  assertBool "2" $ _arc (late  (1/2) p) (-1,1) == [((1/2 , 1  ), ())]
  assertBool "3" $ _arc (slow 2     p) (-1,3) == [((  0 , 2  ), ())]
  assertBool "4" $ _arc (fast 2     p) (-1,3) == [((  0 , 1/2), ())]
  assertBool "5" $ _arc (rev        p) (-2,1) == [(( -1 , 0  ), ())]
  let q = stack (late 1 $ fmap (const 1) p)
                 (late 3 $ fmap (const 2) p)
  assertBool "6" $ _arc (rev q) (-5,1) == [ (( -4 , -3  ), 2)
                                           , (( -2 , -1  ), 1) ]

testApplyEpic = TestCase $ do
  let f1 =          for 1 (+1)
      f2 = late 2 $ for 2 (+2)
      f3 = late 4 $ for 2 (+3)
      g1 = Epic Nothing $ \a -> _arc f1 a ++ _arc f2 a ++ _arc f3 a
      x = loope 2 $ for 1 (1 :: Int)
  assertBool "1" $ _arc (f1 <*> x) (0,4) == [((0,1),2)]
  assertBool "tricky!" $ _arc (g1 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),4) ]
  -- An earlier version of takeOverlappingEvs caused the event at time 4
  -- to undergo both (+2) (with duration 0) and (+3) (with duration 1).
  let g2 = loope 4 g1
  assertBool "3" $ _arc (g2 <*> x) (0,8) ==
    [ ((0,1),2),  ((2,3),3),  ((4,5),2),  ((6,7),3)]
  assertBool "4" $ _period g2 == Just 4
  assertBool "5" $ _period (g2 <*> x) == Just 4
  let f = stack (for 1 (+1)) (late (1/2) $ for 1 (+2))
      x =         for 1 1
  assertBool "overlapping functions induce concurrency"
    $ _arc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f =         for 1 (+1)
      x = stack (for 1 1  ) (late (1/2) $ for 1 2)
  assertBool "overlapping objects are concurrency"
    $ _arc (f <*> x) (0,1) == [((0,1),2),  ((1/2,1),3)]
  let f = stack (for 1 (+1)) (late (1/2) $ for 1 (+2))
      x = stack (for 1 1  ) (late (1/2) $ for 1 2)
  assertBool "concurrent objects and functions multiply like the list monad"
    $ _arc (f <*> x) (0,1)
    == [((0,1), 2),  ((1/2,1),3),  ((1/2,1), 3),  ((1/2,1), 4)]

testApplyEpic2 = TestCase $ do
  let e1 = loopa 1 (+1) <*> (loopa 1 1 +- loopa 1 2)
      e2 =                   loopa 1 2 +- loopa 1 3
  assertBool "works on unit interval" $ _arc e1 (0,1) == _arc e2 (0,1)
  assertBool "<*> fails beyond (0,1)" $ _arc e1 (0,2) == _arc e2 (0,2)

testWindow = TestCase $ do
  let ep = window (2,5) $ loope 2 $ for 1 ()
  assertBool "1" $ _arc ep (2,5) == [((2,3),())
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
      events1 = _arc p1 (0,2)
      a1 = head events1
  assertBool "interval" $ fst a1 == (0,1)
  assertBool "exactly one event" $ length events1 == 1
  assertBool "no overlap, early" $ null $ _arc p1 (-3,-2)
  assertBool "no overlap, late" $ null $ _arc p1 (2,3)
  let events2 = _arc p1 (1/2,3/2)
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
  assertBool "1" $ _arc p (0,4) == [((0,0),()), ((2,2),())]

testLcmRatios = TestCase $ do
  assertBool "1" $ lcmRatios (1%3) (1%6) == (1%3)
  assertBool "2" $ lcmRatios (1%3) (1%2) == 1
