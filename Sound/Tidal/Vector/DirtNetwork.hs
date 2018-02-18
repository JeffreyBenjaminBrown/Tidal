module Sound.Tidal.Vector.DirtNetwork where

import Control.Applicative
import Control.Concurrent
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Map hiding (map, mapMaybe, filter)
import Data.Ord
import Data.Ratio
import Data.Typeable
import Data.Function
import System.Random.Mersenne.Pure64
import qualified Data.Text as T
import Text.Show.Functions ()
import qualified Control.Exception as E

import Sound.Tidal.Dirt (dirt, superDirtBackend, superDirtSlang)
import Sound.Tidal.Tempo (clockedTick, runClient)
import Sound.Tidal.OscStream (makeConnection)
import Sound.Tidal.Pattern (filterOnsetsInRange, seqToRelOnsetDeltas)
import Sound.Tidal.Stream (ticksPerCycle, setter)
import Sound.Tidal.Time
import Sound.Tidal.Transition (transition)
import Sound.Tidal.Utils

import Sound.Tidal.Vector.Types.Reimports hiding (arc)
import Sound.Tidal.Vector.Types
import Sound.Tidal.Vector.DurVec


-- | like superDirtSetters, but for DurVecs, and returning no transitioner
eSuperDirtSetters :: IO Time -> IO (DurVec ParamMap -> IO ())
eSuperDirtSetters getNow = do ds <- eSuperDirtState 57120
                              return $ setter ds

eSuperDirtState :: Int -> IO (MVar (DurVec ParamMap, [DurVec ParamMap]))
eSuperDirtState port = do backend <- superDirtBackend port
                          eStartVoice backend dirt

eStartVoice ::
  Backend a -> Shape -> IO (MVar (DurVec ParamMap, [DurVec ParamMap]))
eStartVoice backend shape = do
  nowAndHistory <- newMVar (silence, [])
  let ot = eOnTick backend shape nowAndHistory :: TimeFrame -> Int -> IO ()
  forkIO $ clockedTick ticksPerCycle ot
  return nowAndHistory

-- | evaluate music between two ticks; send it
-- There are 8 ticksPerBeat, but `tick` does not wrap;
-- it can take any nonnegative value.
-- PITFALL ? Not using second parameter of the Arcs sent via toMessages.
-- From it, SuperCollider once seemed to compute the sustain parameter. If so,
-- every DurVec sent to eOnTick needs to state sustain explicitly.
-- But lately (2017 11 25) I don't see that happening.
eOnTick :: Backend a -> Shape -> MVar (DurVec ParamMap,[DurVec ParamMap])
  -> TimeFrame -> Tick -> IO ()
eOnTick     backend     shape   nowAndHistory         change tick = do
  ps <- readMVar nowAndHistory
  let tick' = fromIntegral tick :: Integer
      a = tick' % ticksPerCycle
      b = (tick' + 1) % ticksPerCycle
      messages = mapMaybe (toMessage backend shape change tick)
                          -- [(0,0.1, fromList [ (s_p,VS "sy")
                          --                   , (sustain_p, VF 1)] )]
                          (dvStartsInArc (a, b) $ fst ps)
  sequence_ messages `E.catch`
    (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
  flush backend shape change tick
  return ()

-- | Samples some events from a DurVec, returning a list of onsets
-- (relative to the given arc), durations (PITFALL: relative to
-- the arc), and values.
dvStartsInArc :: Arc -> DurVec a -> [(Double, Double, a)]
dvStartsInArc (s,e) dv = map f $ arc dv (s,e)
  -- TODO : unfinished : see DurVec.arc / todo
  where f ((s', e'), x) = ( fromRational $ (s'-s) / (e-s)
                          , fromRational $ (e'-s) / (e-s)
                          , x)
