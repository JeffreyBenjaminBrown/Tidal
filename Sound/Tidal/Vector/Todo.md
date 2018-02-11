scales
  parser: accept lists, ratios
  types: use an Epic [Float], so that scales can be subsetted

search for "todo" in the epic branch

startWhenIWant: solutions from Alex
    do cps $ -1
       threadDelay 100000
       cps (160/60/4)

    d1 $ (pure now) ~>
      stack [
         slow 4 $ sound "<fband fband fband:1 fband:2 >"
      ]

scale de-transposer
  Takes an Epic Scale x, returns an Epic (ParamMap -> ParamMap) y,
  such that if y is applied to ParamEpic z, z won't sound transposed
  by the roots of the scale.

x-voice coordination

Music theory- and across voice-aware scale, degree, chord sequence manipulations.

In parsed strings, duration control is still kind of verbose.

t1%2, for 1,2 in [0,9], should affect bracketed exprs, not just lexemes.
