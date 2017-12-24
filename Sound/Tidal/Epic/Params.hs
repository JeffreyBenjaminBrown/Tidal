module Sound.Tidal.Epic.Params (
  -- | == functions for mapping over Params or the Params in Epics
  fpar, mdeg

  -- | ==== Params
  -- | == for the sy and sya synths
  , qf,  qf_p
  , qfa, qfa_p, qff, qff_p
  , qpa, qpa_p, qpf, qpf_p
  , qaa, qaa_p, qaf, qaf_p

  -- | == derivative Params
  , et -- ^ 12 tone equal temperament

  -- | == scale (deg)ree is an intermediate Param; it eventually becomes speed
  , deg_p

  -- | == renamed Params
  , sound_p, sound
  , sample_p, sample

  -- | == Params unchanged from Tidal
  , P.accelerate_p, accelerate
  , P.attack_p, attack
  , P.bandf_p, bandf
  , P.bandq_p, bandq
  , P.begin_p, begin
  , P.bpf_p, bpf
  , P.bpq_p, bpq
  , P.channel_p, channel
  , P.clhatdecay_p, clhatdecay
  , P.coarse_p, coarse
  , P.crush_p, crush
  , P.ctranspose_p, ctranspose
  , P.cutoffegint_p, cutoffegint
  , P.cutoff_p, cutoff
  , P.cut_p, cut
  , P.decay_p, decay
  , P.degree_p, degree
  , P.delayfeedback_p, delayfeedback
  , P.delay_p, delay
  , P.delaytime_p, delaytime
  , P.detune_p, detune
  , P.dry_p, dry
  , P.dur_p, dur
  , P.end_p, end
  , P.expression_p, expression
  , P.gain_p, gain
  , P.gate_p, gate
  , P.harmonic_p, harmonic
  , P.hatgrain_p, hatgrain
  , P.hcutoff_p, hcutoff
  , P.hold_p, hold
  , P.hpf_p, hpf
  , P.hpq_p, hpq
  , P.hresonance_p, hresonance
  , P.kriole_p, kriole
  , P.lagogo_p, lagogo
  , P.lclap_p, lclap
  , P.lclaves_p, lclaves
  , P.lclhat_p, lclhat
  , P.lcrash_p, lcrash
  , P.legato_p, legato
  , P.lfocutoffint_p, lfocutoffint
  , P.lfodelay_p, lfodelay
  , P.lfoint_p, lfoint
  , P.lfo_p, lfo
  , P.lfopitchint_p, lfopitchint
  , P.lfoshape_p, lfoshape
  , P.lfosync_p, lfosync
  , P.lhitom_p, lhitom
  , P.lkick_p, lkick
  , P.llotom_p, llotom
  , P.lock_p, lock
  , P.loop_p, eLoop
  , P.lophat_p, lophat
  , P.lpf_p, lpf
  , P.lpq_p, lpq
  , P.lsnare_p, lsnare
  , P.modwheel_p, modwheel
  , P.mtranspose_p, mtranspose
  , P.nudge_p, nudge
  , P.octave_p, octave
  , P.octaveRatio_p, octaveRatio
  , P.offset_p, offset
  , P.ophatdecay_p, ophatdecay
  , P.orbit_p, orbit
  , P.panorient_p, panorient
  , P.pan_p, pan
  , P.panspan_p, panspan
  , P.pansplay_p, pansplay
  , P.panwidth_p, panwidth
  , P.phaserdepth_p, phaserdepth
  , P.phaserrate_p, phaserrate
  , P.pitch1_p, pitch1
  , P.pitch2_p, pitch2
  , P.pitch3_p, pitch3
  , P.portamento_p, portamento
  , P.release_p, release
  , P.resonance_p, resonance
  , P.room_p, room
  , P.sagogo_p, sagogo
  , P.sclap_p, sclap
  , P.sclaves_p, sclaves
  , P.scrash_p, scrash
  , P.semitone_p, semitone
  , P.shape_p, shape
  , P.size_p, size
  , P.slide_p, slide
  , P.speed_p, speed
  , P.stepsPerOctave_p, stepsPerOctave
  , P.stutterdepth_p, stutterdepth
  , P.stuttertime_p, stuttertime
  , P.sustain_p, sustain
  , P.sustainpedal_p, sustainpedal
  , P.tomdecay_p, tomdecay
  , P.tremolodepth_p, tremolodepth
  , P.tremolorate_p, tremolorate
  , P.unit_p, unit
  , P.vcfegint_p, vcfegint
  , P.vcoegint_p, vcoegint
  , P.velocity_p, velocity
  , P.voice_p, voice
  , P.vowel_p, vowel
  ) where

import qualified Data.Map as M

import Sound.Tidal.Epic.Types.Reimports (Value(..), Param(..), ParamMap(..))
import Sound.Tidal.Epic.Types
import Sound.Tidal.Epic.Instances
import qualified Sound.Tidal.Params as P


iParam :: Param -> Epic Int     -> ParamEpic
iParam p e = fmap (\i -> M.singleton p $ VI i) e
fParam :: Param -> Epic Double  -> ParamEpic
fParam p e = fmap (\f -> M.singleton p $ VF f) e
sParam :: Param -> Epic String  -> ParamEpic
sParam p e = fmap (\s -> M.singleton p $ VS s) e

fpar :: Param -> (Double -> Double) -> ParamMap -> ParamMap
fpar p op m = M.mapWithKey f m where
  f k v = if k == p then let VF old = v in VF $ op old
                    else v

-- | do something to all the degree values in an Epic
mdeg :: (Double -> Double) -> ParamEpic -> ParamEpic
mdeg    f                  =  fmap $ fpar deg_p f

-- | ==== Params new in Tidal.Epic

-- | == params for the sy and sya synths
-- >> TODO: some of these defauls are probably wrong 
(_, qf_p ) = P.pF "qf"  (Just 440)
qf = fParam qf_p
(_, qfa_p) = P.pF "qfa" (Just 1)
qfa = fParam qfa_p
(_, qff_p) = P.pF "qff" (Just 1)
qff = fParam qff_p
(_, qpa_p) = P.pF "qpa" (Just 1)
qpa = fParam qpa_p
(_, qpf_p) = P.pF "qpf" (Just 1)
qpf = fParam qpf_p
(_, qaa_p) = P.pF "qaa" (Just 1)
qaa = fParam qaa_p
(_, qaf_p) = P.pF "qaf" (Just 1)
qaf = fParam qaf_p

-- | scale (deg)ree is an intermediate Param, meaningless to SuperDirt
-- It eventually translates into speed_p.
(_, deg_p)                 = P.pF "deg" (Just 0)
-- to use this: Ignore it if there's no scale pattern.
-- If there's a scale pattern, it must be of type Epic (ParamMap -> ParamMap)

-- | ==== Params from Tidal, modified somehow
-- | == derivative Params
et = speed . ((k**) <$>) where k = 2**(1/12)

-- | == specially named Params
sound_p = P.s_p
sound            = sParam sound_p

sample_p = P.n_p
sample           = iParam sample_p
eLoop            = fParam P.loop_p -- "loop" is used in Epic.Abbreviations

-- | ==== Params from Tidal, roughly unchanged
-- | == String Params
unit                      = sParam P.unit_p
vowel                     = sParam P.vowel_p

-- | == Double Params 
accelerate                = fParam P.accelerate_p
attack                    = fParam P.attack_p
bandf                     = fParam P.bandf_p
bandq                     = fParam P.bandq_p
begin                     = fParam P.begin_p
bpf                       = fParam P.bpf_p
bpq                       = fParam P.bpq_p
clhatdecay                = fParam P.clhatdecay_p
crush                     = fParam P.crush_p
ctranspose                = fParam P.ctranspose_p
cutoffegint               = fParam P.cutoffegint_p
cutoff                    = fParam P.cutoff_p
decay                     = fParam P.decay_p
degree                    = fParam P.degree_p
delayfeedback             = fParam P.delayfeedback_p
delay                     = fParam P.delay_p
delaytime                 = fParam P.delaytime_p
detune                    = fParam P.detune_p
dry                       = fParam P.dry_p
dur                       = fParam P.dur_p
end                       = fParam P.end_p
expression                = fParam P.expression_p
gain                      = fParam P.gain_p
gate                      = fParam P.gate_p
harmonic                  = fParam P.harmonic_p
hatgrain                  = fParam P.hatgrain_p
hcutoff                   = fParam P.hcutoff_p
hold                      = fParam P.hold_p
hpf                       = fParam P.hpf_p
hpq                       = fParam P.hpq_p
hresonance                = fParam P.hresonance_p
lagogo                    = fParam P.lagogo_p
lclap                     = fParam P.lclap_p
lclaves                   = fParam P.lclaves_p
lclhat                    = fParam P.lclhat_p
lcrash                    = fParam P.lcrash_p
legato                    = fParam P.legato_p
lfocutoffint              = fParam P.lfocutoffint_p
lfodelay                  = fParam P.lfodelay_p
lfoint                    = fParam P.lfoint_p
lfo                       = fParam P.lfo_p
lfopitchint               = fParam P.lfopitchint_p
lfoshape                  = fParam P.lfoshape_p
lfosync                   = fParam P.lfosync_p
lhitom                    = fParam P.lhitom_p
lkick                     = fParam P.lkick_p
llotom                    = fParam P.llotom_p
lock                      = fParam P.lock_p
lophat                    = fParam P.lophat_p
lpf                       = fParam P.lpf_p
lpq                       = fParam P.lpq_p
lsnare                    = fParam P.lsnare_p
modwheel                  = fParam P.modwheel_p
mtranspose                = fParam P.mtranspose_p
nudge                     = fParam P.nudge_p
octaveRatio               = fParam P.octaveRatio_p
offset                    = fParam P.offset_p
ophatdecay                = fParam P.ophatdecay_p
panorient                 = fParam P.panorient_p
pan                       = fParam P.pan_p
panspan                   = fParam P.panspan_p
pansplay                  = fParam P.pansplay_p
panwidth                  = fParam P.panwidth_p
phaserdepth               = fParam P.phaserdepth_p
phaserrate                = fParam P.phaserrate_p
pitch1                    = fParam P.pitch1_p
pitch2                    = fParam P.pitch2_p
pitch3                    = fParam P.pitch3_p
portamento                = fParam P.portamento_p
release                   = fParam P.release_p
resonance                 = fParam P.resonance_p
room                      = fParam P.room_p
sagogo                    = fParam P.sagogo_p
sclap                     = fParam P.sclap_p
sclaves                   = fParam P.sclaves_p
scrash                    = fParam P.scrash_p
semitone                  = fParam P.semitone_p
shape                     = fParam P.shape_p
size                      = fParam P.size_p
slide                     = fParam P.slide_p
speed                     = fParam P.speed_p
stepsPerOctave            = fParam P.stepsPerOctave_p
stutterdepth              = fParam P.stutterdepth_p
stuttertime               = fParam P.stuttertime_p
sustain                   = fParam P.sustain_p
sustainpedal              = fParam P.sustainpedal_p
tomdecay                  = fParam P.tomdecay_p
tremolodepth              = fParam P.tremolodepth_p
tremolorate               = fParam P.tremolorate_p
vcfegint                  = fParam P.vcfegint_p
vcoegint                  = fParam P.vcoegint_p
velocity                  = fParam P.velocity_p
voice                     = fParam P.voice_p

-- | == Int Params
channel                   = iParam P.channel_p
coarse                    = iParam P.coarse_p
cut                       = iParam P.cut_p
kriole                    = iParam P.kriole_p
octave                    = iParam P.octave_p
orbit                     = iParam P.orbit_p
