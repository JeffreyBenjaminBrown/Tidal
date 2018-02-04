write an unBreathe, which contracts
  see, e.g., music/notSwing.hs

startWhenIWant:
  Here's a hack. It requires restarting GHCI.
  Run as three separate lines iin GHCI, pasted all at once:
    :s env.hs (without MakeVoices)
    :s ../EpicDemo/MakeVoices.hs
    v0 $ dsh 1 +- pe0 "_cp - /16 **16 _sn"

syFreq, etc.: convert from ParamEpic -> ParamEpic to ParamMap -> ParamMap
  that way I can use it with <$< (fixity 4),
  instead of jumping to fixity=0 with $

scale de-transposer
  Takes an Epic Scale x, returns an Epic (ParamMap -> ParamMap) y,
  such that if y is applied to ParamEpic z, z won't sound transposed
  by the roots of the scale.

x-voice coordination

Music theory- and across voice-aware scale, degree, chord sequence manipulations.

In parsed strings, duration control is still kind of verbose.

Parse rationals from floats, floats from rationals?
  to s=speed, add sr="speed as rational", etc.

t1%2, for 1,2 in [0,9], should affect bracketed exprs, not just lexemes.
