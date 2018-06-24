scales
  parser: accept lists, ratios
  types: use an Epic [Float], so that scales can be subsetted

legato: compute duration within patterns
  and tie it to cps somehow
  and let operators like fast change it
  and still it'll fuck up if I slow down only part of a pattern

In ptm, ability to target transformations to specific samples
  EDIT: or scale degrees
    I don't remember whether ptm is scale degree-aware, but something is

- < -- < ||| < ---- etc.

dj ? include a second map, from names to transformations

search for "todo" in the code

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

tx%y, for x,y in [0,9], should affect bracketed exprs, not just lexemes.
