-- Let's Stay Together, by Al Greene

cps $ 5/3 -- the song's a little faster than this, but this is easier
toms = fast 2 $ pe0 "_ [**2 _mt]" &* pe "g0.8"
bdsn = pe0 "_bd _sn [*2 _bd _bd,1s0.8,1g0.9] _sn"
bdsn2 = pe0 "_bd,1s0.8 | _sn,g0.9"

hat8  = pe0 ">1%2 [t7,_ - t1%2,_ho _hc]"
hat16 = pe0 ">1%2 [t15,_ - t1%2,_ho _hc]"
hat32 = pe0 ">1%2 [t31,_ - t1%2,_ho _hc]"

bass pat = syFuzzBass &* pe "sus0.8"
  &* remapPd qf_p
  <$< (\x->2**(x/12))
  <$< pat
bassVerse = bass $ fast 2 $ pd0
  "  0 _ _  2  4  _ 7  _  -   0  _ _ 2  4 _ 7 9 \
  \ -3 _ _ -3 -8  _ -5 _  -  -3 _ _ -8 -3 _  2 _ \
  \ -7 _ _ -5 -3  _ 0  _  -  -7 _ _ -5 -3 -7 0 _ \
  \ -4 _ _ -4 -4  _ _ -7  -  -4 _ _ -4 -4 _ -7 _ "
bassBridge = bass $ fast 2 $ pd0
  "  -8 _ _  4  2 _ _  2   -   0  _ _  0  -1 _  -1  _ \
  \  -3 _ _ -3 -3 _ _ -3   -  -3  _ _ -3  -3 -1  0  2 \
  \  4 _ _  4   2 _ _  2   -   0  _ 2  3   4 -8 -1 -2 \
  \  -3 _ _ -3 -3 _ _ -3   -  -3  _ _ -3  -3 _  -5 _ "
bassChor = bass $ fast 2 $ pd0
  " 2 _ _  -3  -  2 2 _ -3  -  2   _ _  -3  -  2  2 _ -1 \
  \ 4 _ _  -1  -  4 4 _ -1  -  4   _ _  -1  -  4  4 _ -3 \
  \ 2 _ _  -3  -  2 2 _ -3  -  2   _ _  -3  -  2  2 _ -1"
bassHappy = bass $ fast 2 $ pd0
  " 5 5 _   5  -  4 4 _  4  -  -3 -3 _  -3  -  7 -1 0 4"
bassWeird = bass $ fast 2 $ pd0
  " 2 2 _ [*2 _ -3]  -  2 2 _ -3  -  2 2 _ [*2 _ -3]  -  2 _ 2 _ \
  \ 3 3 _ [*2 _  0]  -  3 3 _  _                                 \
  \ 2 2 _ [*2 _ -3]  -  2 2 _ -2                                 \
  \ 3 3 _ [*2 _  0]  -  3 3 _  3  -  3 3 _ [*2 _  0]  -  3 _ 3 _ "

verse  = bdsn                        +| toms +| hat32 +| bassVerse
bridge = bdsn2                       +| toms +| hat16 +| bassBridge 
chor    = bdsn  +| toms +| hat8 +| bassChor
happy   = bdsn2 +| toms +| hat8 +| bassHappy
weird   = bdsn  +| toms +| bassWeird

v1 $ verse +- bridge +- chor +- happy +- weird +- happy
  +- verse +- bridge +- chor +- happy +- chor  +- happy
