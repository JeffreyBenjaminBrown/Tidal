{-# LANGUAGE ViewPatterns #-}

module Sound.Tidal.Epic.Scale where

-- for these same scales organized as a map,
-- see /git_play/tidal/tidal-extensions.hs

import qualified Data.Map as M

import           Sound.Tidal.Epic.Instances
import           Sound.Tidal.Epic.Params
import           Sound.Tidal.Epic.Util (mergeNumParamsWith)
import           Sound.Tidal.Epic.Transform (chParam, chVF)
import           Sound.Tidal.Epic.Types
import qualified Sound.Tidal.Params         as P
import           Sound.Tidal.Epic.Types.Reimports


transposeScale :: Double -> Scale -> Scale
transposeScale d s = transp . s -- transpose 2nd; else might have no speeds
  where transp = mergeNumParamsWith (*) (*) (M.singleton speed_p $ VF d)

-- | ==== looking up scale degrees
-- | == 12 tone scales
remUnif :: Integral a => a -> a -> a
remUnif num den = -- positive remainder
  if x<0 then x+den else x where x = rem num den
  -- fmap (flip remUnif 3) [-5..5] -- test
  -- ifdo speed: could use one divide instead of many adds

quotUnif :: Integral a => a -> a -> a
quotUnif num den = if num < 0 then q - 1 else q
  where q = quot num den

-- | for synths, convert speed_p parameter to qf_p (frequency) parameter,
-- or gain_p to amp_p, or both.
-- TODO ? why is amp louder than gain? These are all equally loud:
  -- v0 $ pe0 "_sy,f440,sus1"
  -- v0 $ pe0 "_sy,f440,sus1,g1"
  -- v0 $ pe0 "_sy,f440,sus1,a0.42" -- but amp defaults to 1 in SuperCollider!
syParams :: ParamEpic -> ParamEpic
syParams = fmap $ chParam speed_p qf_p . chParam gain_p amp_p
syFreq   :: ParamEpic -> ParamEpic
syFreq   = fmap $ chParam speed_p qf_p
syAmp    :: ParamEpic -> ParamEpic
syAmp    = fmap $                        chParam gain_p amp_p

-- | parameter lookup. it's like lk12', but applied to deg_p.
par12 :: [Double] -> (ParamMap -> ParamMap)
par12 scale map = mergeNumParamsWith (*) (*) mOtherParams $ g mDegree where
  (mDegree,mOtherParams) = M.partitionWithKey f map
  f :: Param -> Value -> Bool
  f k _ = if k == deg_p then True else False
    -- PITFALL: It seems like "f deg_p _ = True; f _ _ = False"
    -- ought to work, but somehow no.
  g :: ParamMap -> ParamMap
  g = chParam deg_p speed_p . chVF deg_p rescale where
    k = 2**(1/12)
    rescale :: Double -> Double
    rescale deg = k ** lk12' scale deg

lk12 :: Epic [Double] -> Epic (Double -> Double)
lk12 = fmap lk12'

lk12' :: [Double] -> Double -> Double
lk12' scale i = let (a,b) = (floor i, ceiling i)
                    rem = i - fromIntegral a
                    (a',b') = (lkInt12' scale a, lkInt12' scale b)
                in a' + rem * (b'-a')

lkInt12 :: Epic [Double] -> Epic (Int -> Double)
lkInt12 = fmap lkInt12'

lkInt12' :: [Double] -> Int -> Double -- lookup an int in a 12 tone scale
lkInt12' sc idx =
  let len = length sc
      idx' = floor $ fromIntegral $ remUnif idx len
      octaves = quotUnif idx len
  in (12 * fromIntegral octaves) + (sc !! idx')

-- | == equal-tempered scales

-- | this is like lkInt12'; it could support a similar stack of functions
lkIntJi' :: [Double] -> Int -> Double -- lookup an int in a just scale
lkIntJi' sc idx =
  let len = length sc
      idx' = floor $ fromIntegral $ remUnif idx len
      octaves = quotUnif idx len
  in (2 ** fromIntegral octaves) * (sc !! idx')


-- | ==== scales
-- | == symmetric scales
dim_s = [0,2,3,5,6,8,9,11] -- diminished up
dim = par12 dim_s
dim' = lk12' dim_s
dimd_s = [0,1,3,4,6,7,9,10] -- diminished down
dimd = par12 dimd_s
dimd' = lk12' dimd_s
aug_s = [0,3,4,7,8,11] -- augmented up
aug = par12 aug_s
aug' = lk12' aug_s
augd_s = [0,1,4,5,8,9] -- aug down
augd = par12 augd_s
augd' = lk12' augd_s
hol_s = [0,2,4,6,8,10] -- whole tone
hol = par12 hol_s
hol' = lk12' hol_s

-- | == diatonic family
maj_s = [0,2,4,5,7,9,11] -- major = ionian
maj = par12 maj_s
maj' = lk12' maj_s
dor_s = [0,2,3,5,7,9,10] -- dorian
dor = par12 dor_s
dor' = lk12' dor_s
phr_s = [0,1,3,5,7,8,10] -- phrygian
phr = par12 phr_s
phr' = lk12' phr_s
lyd_s = [0,2,4,6,7,9,11] -- lydian
lyd = par12 lyd_s
lyd' = lk12' lyd_s
mix_s = [0,2,4,5,7,9,10] -- mixolydian
mix = par12 mix_s
mix' = lk12' mix_s
aol_s = [0,2,3,5,7,8,10] -- aeolian
aol = par12 aol_s
aol' = lk12' aol_s
loc_s = [0,1,3,5,6,8,10] -- locrian
loc = par12 loc_s
loc' = lk12' loc_s

-- | == harmonic minor family
maj5_s = [0,2,4,5,8,9,11] -- major #5
maj5 = par12 maj5_s
maj5' = lk12' maj5_s
dor4_s = [0,2,3,6,7,9,10] -- dorian #4
dor4 = par12 dor4_s
dor4' = lk12' dor4_s
phr3_s = [0,1,4,5,7,8,10] -- phrygian #3
phr3 = par12 phr3_s
phr3' = lk12' phr3_s
lyd2_s = [0,3,4,6,7,9,11] -- lydian #2
lyd2 = par12 lyd2_s
lyd2' = lk12' lyd2_s
loc47_s = [0,1,3,4,6,8,9] -- locrian b4b7
loc47 = par12 loc47_s
loc47' = lk12' loc47_s
aol7_s = [0,2,3,5,7,8,11] -- aeolian #7
aol7 = par12 aol7_s
aol7' = lk12' aol7_s
loc6_s = [0,1,3,5,6,9,10] -- locrian #6
loc6 = par12 loc6_s
loc6' = lk12' loc6_s

-- | == harmonic minor family backward
maj6_s = [0,2,4,5,7,8,11] -- major b6
maj6 = par12 maj6_s
maj6' = lk12' maj6_s
dor5_s = [0,2,3,5,6,9,10] -- dorian b5
dor5 = par12 dor5_s
dor5' = lk12' dor5_s
phr4_s = [0,1,3,4,7,8,10] -- phrygian b4
phr4 = par12 phr4_s
phr4' = lk12' phr4_s
lyd3_s = [0,2,3,6,7,9,11] -- lydian b3
lyd3 = par12 lyd3_s
lyd3' = lk12' lyd3_s
mix2_s = [0,1,4,5,7,9,10] -- mixolydian b2
mix2 = par12 mix2_s
mix2' = lk12' mix2_s
lyd25_s = [0,3,4,6,8,9,11] -- lydian #2#5
lyd25 = par12 lyd25_s
lyd25' = lk12' lyd25_s
loc7_s = [0,1,3,5,6,8,9] -- locrian b7
loc7 = par12 loc7_s
loc7' = lk12' loc7_s

-- | == ("ascending") melodic minor family
-- all but two of these scales have two reasonable names
maj3_s = [0,2,3,5,7,9,11] -- major b3
maj3 = par12 maj3_s
maj3' = lk12' maj3_s
dor7_s = [0,2,3,5,7,9,11] -- a.k.a. dorian #7
dor7 = par12 dor7_s
dor7' = lk12' dor7_s
dor2_s = [0,1,3,5,7,9,10] -- dorian b2
dor2 = par12 dor2_s
dor2' = lk12' dor2_s
phr6_s = [0,1,3,5,7,9,10] -- a.k.a. phrygian #6
phr6 = par12 phr6_s
phr6' = lk12' phr6_s
lyd5_s = [0,2,4,6,8,9,11] -- lydian #5
lyd5 = par12 lyd5_s
lyd5' = lk12' lyd5_s
lyd7_s = [0,2,4,6,7,9,10] -- lydian b7
lyd7 = par12 lyd7_s
lyd7' = lk12' lyd7_s
mix4_s = [0,2,4,6,7,9,10] -- a.k.a. mixolydian #4
mix4 = par12 mix4_s
mix4' = lk12' mix4_s
mix6_s = [0,2,4,5,7,8,10] -- mixolydian b6
mix6 = par12 mix6_s
mix6' = lk12' mix6_s
aol3_s = [0,2,4,5,7,8,10] -- a.k.a. aeolian #3
aol3 = par12 aol3_s
aol3' = lk12' aol3_s
aol5_s = [0,2,3,5,6,8,10] -- aeolian b5
aol5 = par12 aol5_s
aol5' = lk12' aol5_s
loc2_s = [0,2,3,5,6,8,10] -- a.k.a. locrian #2
loc2 = par12 loc2_s
loc2' = lk12' loc2_s
loc4_s = [0,1,3,4,6,8,10] -- locrian b4
loc4 = par12 loc4_s
loc4' = lk12' loc4_s
