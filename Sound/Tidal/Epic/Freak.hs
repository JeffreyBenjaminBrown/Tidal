Surprises
  sparse and dense only differ from fast and slow
    when used on an argument to +-

Problems
  within a string there should be no +-, just ,,

Bugs
  Single sample strings parse as epics that are way too fast.
    v1 $ pe "_cp"
  Disappearing notes:
    This snare disappears:
      v1 $   pe "_bd ,, _sn" +- sound (loopa 1 "cp")
    even though eArc sees it:
      eArc (pe0 "_bd +- _sn" +- sound (loopa 1 "cp")) (0,3)
        [((0 % 1,0 % 1),fromList [(s,bd)])
        ,((1 % 1,1 % 1),fromList [(s,sn)])
        ,((2 % 1,3 % 1),fromList [(s,cp)])]

