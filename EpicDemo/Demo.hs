-- | How to run this demo:
  -- You don't need Emacs or Atom or any other editor
    -- although you might use one to paste lines into GHCI.
  -- Install Stack (the Haskell tool) and Git, if either is missing.
  -- Clone the repo: `git clone https://github.com/JeffreyBenjaminBrown/Tidal`
  -- Checkout this branch: `git checkout parseOps`
  -- Run `stack ghci`.
  -- From within GHCI, call `:s EpicDemo/Demo.hs`.
  -- That will create a lot of noise, because it runs this whole file.
  -- To get a better sense of what's going on, it's probably more useful to
  -- step through the file one line at a time, by copying them into GHCI.

:m Sound.Tidal.Epic
import qualified Data.Map as M
:s EpicDemo/MakeVoices.hs

b = 1/4
can' = loope b can  -- a (1/4)-second loop of the can sample
cp' = loope b cp -- a (1/4)-second loop of the clap sample
p1 = cat $ repli 4 can' -- 4 can's; 1 second long
q1 = cat $ repli 3 cp' -- 3 clap's; 3/4 seconds long
f1 = gain $ cata b $ map (*0.7) [0.8, 0.6, 1]
  -- a 3/4-second loop of gain changes
v0 $ p1 +- q1 &+ f1 -- in voice 0, play p1, then play the merge of q1 and f1

arpEpic = cat $ repli 4 $ loope b arp
  -- 4 bow samples over one second
numEpic = cat $ map (loopa b) [1,1.5,2,2.5]
  -- 4 numbers over a second
funcEpic = loopa 1 id +- loopa 1 (+0.75)
  -- a 2-second pattern: id for a second, then add 0.75
v1 $ arpEpic &+ speed (funcEpic <*> ((*3) <$> numEpic))
  -- play arpEpic, merging it with a speed Epic which contains
  -- funcEpic applied to numEpic
v2 $ et (cat0 b $ map (+19) [0,3,7,12]) &+ sound (ever "arp")
  -- a minor equal-tempered arpeggio, 2 octaves up
v2 $ (et $ fmap (+19) $ cat0 b [0,3,7,12]) &+ sound (ever "arp")
  -- equivalent

v3 $ slow 2 $ sound (ever "arp") &+ (et $ (+12) <$> (cata (b*8) [maj6'] <*> cat0 b ((+7) <$> [0,1,1.5,2])))
  -- go between scale steps (the 1.5)

-- using the Double -> Double scales (e.g. maj6', dor4')
b = 1/8
melody = cat0 b [0,2,3,2] +- cat0 (2*b) [5,6]
scaledMelody = et $ (+12) <$> (cata (b*8) [maj6',dor4'] <*> melody)
v3 $ sound (ever "arp") &+ scaledMelody &* speed 2

-- using the ParamMap -> ParamMap scales (e.g. maj6, dor4)
melody = pe0 "t1%8,,d0 d2 d3 d2 t1%4,,d5 d6"
scaleCycle = cata (b*8) [dim,lyd,aug]
scaledMelody = scaleCycle <*> melody
v3 $ sound (ever "arp") &* scaleCycle <*> melody &* speed 2

-- | = using the sy and sya synths

-- NOTE: sy needs an explicit sustain value, else it's inaudible
v4 $ pe0 "_sy,,sus0.7"
v4 $ (syParams $ ps "//4 maj 6dor" <*> pe0 "**2 d0 +- *2 d2 d3 +- **16%3 d4") &* sustain 2 &* qfa 1 &* qf 220 &* sound (ever "sy")
-- NOTE: The following are happily equivalent (they differ at s200 v. f200)
v1 $ syFreq $ ps "maj aug" <*> pe0 "*4 d0 d2 d4 d6" &* pe "_sy,,sus0.6,,s200"
v1 $ syFreq $ ps "maj aug" <*> pe0 "*4 d0 d2 d4 d6" &* pe "_sy,,sus0.6,,f200"
