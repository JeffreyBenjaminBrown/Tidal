The "DJ" family of parsers provide a language for applying transformations to a collection of patterns. Here is an example:

```
pat1 = pe0 "*2 _bd _sn"
pat2 = pe0 "_hc"
aMap = M.fromList [("pat1",pat1),("pat2",pat2)]
aPat = dj (pdjm "   /3 @pat1  \
                  \ - [(g0.9),@pat1 | ea1%4,fa2,@pat2]") aMap
v0 $ aPat
```

`pat1 = pe0 "*2 _bd _sn"` creates a pattern (called `pat1`) that plays the bass drum, then the snare drum. Ordinarily, since it's got two samples, that would take two cycles; the `*2` speeds it up to fit in a single cycle.

`pat2 = pe0 "_hc"` creates a pattern (called `pat2`) that just plays the closed high hat once per cycle.

`aMap = M.fromList [("pat1",pat1),("pat2",pat2)]` just defines a map (called `Map`) that maps the names "pat1" and "pat2" to `pat1` and `pat2`. That's kind of an annoying thing to type, but you don't have to: evaluating `mapNames "pat1 pat2"` in GHCI will copy `aMap = M.fromList [("pat1",pat1),("pat2",pat2)]` to your clipboard (assuming you've got xclip installed).

```
aPat = dj (pdjm
  "   /3 @pat1 - [(g0.9,s1.5),@pat1 | ea1%4,fa2,@pat2]") aMap
```
uses the DJ language. It creates a pattern (called `aPat`) that first spends three cycles (`/3`) playing `pat1`. It then (`-`) spends a single cycle (between the `[` and `]` symbols) playing `pat1` at a gain value of 0.9 and a speed value of 1.5 (`g0.9,s1.5`) while simultaneously (`|`) playing `pat2` a quarter of a cycle early (`ea1%4`) and twice as fast (`fa2`). `pdjm` parses the pattern of instructions; `dj` applies that pattern to `aMap`.

Finally, `v0 $ aPat` plays `aPat` through the voice `v0`.

`pdjm` is specialized to apply to `ParamMap` patterns; it lets you change `ParamMap` values as in (`g0.9,s1.5`) above. There's also `pdj`, which does not let you change parameter values, but which can be applied to more general kinds of patterns. The `dj` function can be applied to the output of either parsers.
