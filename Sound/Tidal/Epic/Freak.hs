Bugs
  +- only keeps the first and last notes.
    The problem
      The hi-hats disappear:
        let x = fast 4 $ pe "_bd ,, _hc ,, _hc ,, _bd"
        v1 x
      even though eArc sees them:
        plist $ eArc x (0,1)
    Maybe solutions
      THIS IS IT:
        try evaluating smaller arcs, and not starting at 0
      look for laziness fixes
        search for "seq" here: https://wiki.haskell.org/Foldr_Foldl_Foldl%27

Todo
  within a string there should be no +-, just ,,

Surprises
  sparse and dense only differ from fast and slow
    when applied to an argument to +-
  Single sample strings parse as epics that are way too fast.
    v1 $ pe "_cp"
    Use pe0 to avoid this.
    It corresponds to the (absent in Epic) requirement that in Sound.Tidal,
      the first pattern dictates the rhythm.

