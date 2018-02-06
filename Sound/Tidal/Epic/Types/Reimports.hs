module Sound.Tidal.Epic.Types.Reimports (
  -- | = from Sound.Tidal.Tempo, plus synonyms
  Tempo
  , TimeFrame
  , anchorInSeconds
  , anchorInBeats

  -- | = from Sound.Tidal.Stream
  , ToMessageFunc(..) -- ^ OscStream.makeConnection clarifies ToMessageFunc
  , Backend(..)
  , Param(..)
  , Shape(..)
  , Value(..)
  , ParamType(..)
  , ParamMap
  , ParamPattern

  -- | = from Sound.Tidal.Time, plus synonyms
  , Time
  , Arc
  , Event

  , Seconds
  , Beats
  , BeatFrac
  , BPS
  , Tick
  , TPB
  , Dur

  -- | = from Sound.Tidal.Pattern
  , Pattern(..)
  ) where

-- These include functions for historical reasons.
-- I've commented those out but left them in place in case they prove needed.

import Data.Time (UTCTime)

import Sound.Tidal.Tempo (Tempo(..))
import Sound.Tidal.Stream (
  ToMessageFunc(..) -- ^ OscStream.makeConnection clarifies ToMessageFunc
  , Backend(..)
  , Param(..)
  , Shape(..)
  , Value(..)
  , ParamType(..)
  , ParamMap
  , ParamPattern
  )
import Sound.Tidal.Time (Time, Arc, Event)
                        --, hasOnset, hasOffset, isIn, subArc)
import Sound.Tidal.Pattern (Pattern(..))
  -- , noOv, showTime, showArc, showEvent, silence, overlay, unwrap)

-- | Tempo = TimeFrame.
-- | If I had my druthers, the relevant names and type synonyms would be:
--   data TimeFrame = TimeFrame {
--     anchorInSeconds :: UTCTime
--     , anchorInBeats :: Beats
--     , cps :: BPS
--     , paused :: Bool
--     , clockLatency :: Seconds}
type TimeFrame = Tempo

anchorInSeconds :: TimeFrame -> UTCTime
anchorInSeconds = at

anchorInBeats :: TimeFrame -> Beats
anchorInBeats = beat

-- | = More time types
type Seconds = Double -- ^ any real value.
  -- seems to ecompasse UTCTimes -- e.g. as used in Sound.Tidal.Stream.doAt
type Beats = Double -- ^ any real value
type BeatFrac = Double -- ^ only the part after the decimal; valued in [0,1)
  -- noteworthy in that it is never used (to my knowledge); all beat-measured
  -- values can be greater than 1
type BPS = Double -- ^ beats per second
type Tick = Int -- ^ in [0,7]
type TPB = Int -- ^ ticks per beat
type Dur = Time
