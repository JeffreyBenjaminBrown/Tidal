Surprises
  sparse and dense only differ from fast and slow
    when applied to an argument to +-
  Single sample strings parse as epics that are way too fast.
    v1 $ pe "_cp"
    Use pe0 to avoid this.
    It corresponds to the (absent in Epic) requirement that in Sound.Tidal,
      the first pattern dictates the rhythm.

Problems
  within a string there should be no +-, just ,,

Bugs
  +- only keeps the first and last notes.
    This snare disappears:
      let x = pe "_bd ,, _sn" +- sound (loopa 1 "cp")
      v1 x
    even though eArc sees it:
      eArc x (0,3)
        [((0 % 1,0 % 1),fromList [(s,bd)])
        ,((1 % 1,1 % 1),fromList [(s,sn)])
        ,((2 % 1,3 % 1),fromList [(s,cp)])]
    and
      y = pe "_bd ,, _sn ,, _cp"
