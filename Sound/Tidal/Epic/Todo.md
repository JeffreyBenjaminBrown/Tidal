Parse patterns of numbers.
  One could then map, say, `early` across the pattern.
  > x = early <$> loopa 1 1
  > :t x
  x :: Epic (Epic a -> Epic a)

t1%2, for 1,2 in [0,9], should affect bracketed exprs, not just lexemes.

Parser (Epic (Map String Value))
  and Map String Param -> Epic (Map String Value) -> Epic (Map Param Value)

Music theory- and across voice-aware scale, degree, chord sequence manipulations.