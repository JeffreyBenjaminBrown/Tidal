module Sound.Tidal.Epic.Harmony where

import           Data.Fixed (div', mod')

import           Sound.Tidal.Epic.Types
import           Sound.Tidal.Epic.Instances
import           Sound.Tidal.Epic.Scale
import           Sound.Tidal.Epic.Params


scaleFromHarmony :: Harmony -> Scale
scaleFromHarmony h = transposeScale (fromRational $ root h) (baseScale h)

score ::      Harmony -> [Degree] -> Degree -> [ScoreRule] -> Degree -> Score
score                h others orig rules d = sum $ map f rules where
  f rule = scoreTerm h others orig rule  d
  scoreTerm ::  Harmony -> [Degree] -> Degree -> ScoreRule -> Degree -> Score
  scoreTerm h others orig rule d = case rule of
    (InScale p) -> if abs (fromIntegral (round d) - d) < 0.01 then 0 else p
    (InChord p) -> if elem (mod' d $ fromIntegral $ scaleSize h) (chord h)
                   then 0 else p
    (DegDiff p) -> abs (orig - d) * p
    (Unique p)  -> if elem d others then p else 0

-- model something on Epic.Scale.par12
