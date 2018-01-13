module Sound.Tidal.Epic.DirtNetwork where

import Control.Applicative
import Control.Concurrent
--import Control.DeepSeq (deepseq) -- TODO: try using
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
import Sound.Tidal.Epic.Types.Reimports hiding (arc)
import Sound.Tidal.Epic.Types
import Sound.Tidal.Utils
import Sound.Tidal.Epic.Instances
import Sound.Tidal.Epic.Transform


-- | like superDirtSetters, but for Epics, and returning no transitioner
eSuperDirtSetters :: IO Time -> IO (ParamEpic -> IO ())
eSuperDirtSetters getNow = do ds <- eSuperDirtState 57120
                              return $ setter ds

eSuperDirtState :: Int -> IO (MVar (ParamEpic, [ParamEpic]))
eSuperDirtState port = do backend <- superDirtBackend port
                          eStartVoice backend dirt

eStartVoice :: Backend a -> Shape -> IO (MVar (ParamEpic, [ParamEpic]))
eStartVoice backend shape = do
  epicsM <- newMVar (silence, [])
  let ot = eOnTick backend shape epicsM :: TimeFrame -> Int -> IO ()
  forkIO $ clockedTick ticksPerCycle ot
  return epicsM

-- | evaluate music between two ticks; send it
-- There are 8 ticksPerBeat, but `tick` does not wrap;
-- it can take any nonnegative value.
-- PITFALL ? Not using second parameter of the Arcs sent via toMessages.
-- From it, SuperCollider once seemed to compute the sustain parameter. If so,
-- every Epic sent to eOnTick needs to state sustain explicitly.
-- But lately (2017 11 25) I don't see that happening.
eOnTick :: Backend a -> Shape -> MVar (ParamEpic,[ParamEpic])
  -> TimeFrame -> Tick -> IO ()
eOnTick     backend     shape          epicsM         change tick = do
  ps <- readMVar epicsM
  let tick' = fromIntegral tick :: Integer
      a = tick' % ticksPerCycle
      b = (tick' + 1) % ticksPerCycle
      messages = mapMaybe (toMessage backend shape change tick)
                          -- [(0,0.1, fromList [ (s_p,VS "sy")
                          --                   , (sustain_p, VF 1)] )]
                          (eSeqToRelOnsetDeltas (a, b) $ fst ps)
  sequence_ messages `E.catch`
    (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
  flush backend shape change tick
  return ()

-- | Samples some events from an Epic, returning a list of onsets
-- (relative to the given arc), durations (PITFALL: relative to
-- the arc), and values.
eSeqToRelOnsetDeltas :: Arc -> Epic a -> [(Double, Double, a)]
eSeqToRelOnsetDeltas (s, e) ep = map f $ filter onsetInRange $ arc ep (s,e)
  where f ((s', e'), x) = ( fromRational $ (s'-s) / (e-s)
                          , fromRational $ (e'-s) / (e-s)
                          , x)
        onsetInRange ((s',_),_) = s <= s' && s' <= e
-- AMBITION ? Rather than filter by onsetInRange, could report only portions
  -- of events that overlap (s,e), whether or not they started recently.
  -- Could matter when the synths are playing notes that vary over time.
