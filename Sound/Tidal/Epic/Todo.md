BUG in rev: in unit length loop, events at 0, 1/2 become events at 1/2, 1
  SOLUTION
    3 - rather than (-e,-s), evaluate (-e,-s+0%99).
          Then throw out anything starting at -e or strictly after -s.
  Bad Solutions
    1 - If event time is equal to 0 modulo epic's duration,
          then bump it backward one duration.
      But what if that puts it out of the interval of evaluation?
        e.g. evaluate from 9 to 10, period=5, so bump 10 to 5, out of bounds.
    2 - If event time is equal to end of interval of evaluation,
          then bump it back to the start of that interval.
      But then where it falls depends on the interval of evaluation.
  Description
    the one that happens on 1/2 is fine, but
    the reversed event that happens on 1 ought to be on 0
    the result: double events, which are extra loud

In ptm, ability to target transformations to specific samples

- < -- < ||| < ---- etc.

legato: compute duration within patterns
  and tie it to cps somehow
  and let operators like fast change it
  and still it'll fuck up if I slow down only part of a pattern

dj ? include a second map, from names to transformations

scales
  parser: accept lists, ratios
  types: use an Epic [Float], so that scales can be subsetted

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

t1%2, for 1,2 in [0,9], should affect bracketed exprs, not just lexemes.
