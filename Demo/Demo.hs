-- | What I do to run this demo:
  -- Clone the repo: `git clone https://github.com/JeffreyBenjaminBrown/Tidal`
  -- Checkout this branch: `git checkout epic`
  -- Run `stack ghci`. (You'll need to have installed Stack.)
  -- From within GHCI, call `:s Sound/Tidal/Epic/Demo.hs`.

:m Sound.Tidal.Epic
import qualified Data.Map as M
:s Demo/MakeVoices.hs

b = 1/4
can' = loope b can  -- a (1/4)-second loop of the can sample
clap' = loope b clap -- a (1/4)-second loop of the clap sample
p1 = cat $ repli 4 can' -- 4 can's; 1 second long
q1 = cat $ repli 3 clap' -- 3 clap's; 3/4 seconds long
f1 = gain $ cata b $ map (*0.7) [0.8, 0.6, 1]
  -- a 3/4-second loop of gain changes
v0 $ p1 +- q1 &+ f1 -- in voice 0, play p1, then play the merge of q1 and f1

bowEpic = cat $ repli 4 $ loope b bow
  -- 4 bow samples over one second
numEpic = cat $ map (loopa b) [1,1.5,2,2.5]
  -- 4 numbers over a second
funcEpic = loopa 1 id +- loopa 1 (+0.75)
  -- a 2-second pattern: id for a second, then add 0.75
v1 $ bowEpic &+ speed (funcEpic <*> numEpic)
  -- play bowEpic, merging it with a speed Epic which contains
  -- funcEpic applied to numEpic
v2 $ et (cat0 b $ map (+19) [0,3,7,12]) &+ sound (ever "bow")
  -- a minor equal-tempered arpeggio, 2 octaves up
v2 $ (et $ fmap (+19) $ cat0 b [0,3,7,12]) &+ sound (ever "bow")
  -- equivalent

v3 $ slow 2 $ sound (ever "bow") &+ (et $ (+12) <$> (cata (b*8) [maj6'] <*> cat0 b ((+7) <$> [0,1,1.5,2])))
  -- go between scale steps (the 1.5)

-- using the Double -> Double scales (e.g. maj6', dor4')
b = 1/8
melody = cat0 b [0,2,3,2] +- cat0 (2*b) [5,6]
scaledMelody = et $ (+12) <$> (cata (b*8) [maj6',dor4'] <*> melody)
v3 $ sound (ever "bow") &+ scaledMelody &* speed 2

-- using the ParamMap -> ParamMap scales (e.g. maj6, dor4)
melody = p0 " t1%8 d0 ,, d2 ,, d3 ,, d2 ,, t1%4 d5,,d6"
scaleCycle = cata (b*8) [dim,lyd,aug]
scaledMelody = scaleCycle <*> melody
v3 $ sound (ever "bow") &* scaleCycle <*> melody &* speed 2

-- using the sy and sya synths
v4 $ (syFreq $ scaleCycle <*> melody) &* sustain 2 &* qfa 1 &* qf 440 &* sound (ever "sy")
