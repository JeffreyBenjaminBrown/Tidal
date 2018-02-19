-- bassline modeled after the one in Soul Coughing's "Blame"

hush
v0 $ meta (pt "/4 id ea1")
  $ meta (pt "ea1%3 fa3 | /2 id shh]")
  (pe0 "_mt,s1.2,g0.8  \
  \ | s1 s1.5    \
  \ | pan0 [*2 pan1 pan-1] pan1")
v1 $ meta (pt "/4 [*//2 ea1%6 la1%6 - shh] | id") $ pe0 "*3%2 _bd _sn"
v2 $ syHollowBass
  &* (fast 6 $ pe0 " /**3 f1 - /2 fr9%7 fr7%4 - fr4%3 - \
                   \ /3 fr3%2 - /3 fr9%8 - /2 fr7%8 -   \
                   \ /3 f1 - /2 [**2 f1.5] fr7%4 - /8 _")
  &* slow 12 (pe "[**12 f1] [**12 fr7%8]")

hush
v1 $ pe "_sy,sus1,f402" &* pe "fr2,g0.73"
   &* meta (pt "fa2 rev | ea1%2") (pe "fr9%7 f1%2 [*//2 f1 fr3%2]")
   &* (fast 2 $ meta (pt "fa2 rev | ea1%2")
       (pe0 "f1 fr4%5 [*//2 f2,g0.8 fr3%2]"))
   &* pe "/12 f1 fr7%8 | f1 fr7%2,g0.8"
v2 $ syFuzzBass' &* pe "f100.5,sus0.5,g1.2"
  &* (fast 8 $ pe0 " /**3 f1 - /2 fr9%7 fr7%4 - fr4%3 - \
                   \ /3 fr3%2 - /3 fr9%8 - /2 fr7%8 -   \
                   \ /3 f1 - /2 [**2 f1.5] fr7%4 - /8 _")
  &* slow 12 (pe "[**12 f1] [**12 fr7%8]")

v0 $ meta (pt "fa2,ea1%4 fa4 | /2 id shh]")
  (pe0 "_bd,1g1.2 [*//2 _sn,1:0 1:2]")
v3 $ meta (pt "fa2,ea1%4 fa4 | /2 id shh]")
  (pe0 "<1%2 *2 [*//2 _can,1:2 [*//2 _hc,s1 1s2]] _mt,s1.2  \
  \ | pan0 [*2 pan1 pan-1] pan1")

hush
v0 $ meta (pt "/16 fa2 id sl2 id")
  $ meta (pt "/4 [*//4 ea1%4 la3%4 - shh | fa2 id] | /2 <1%2 fa2 id")
  $ pe0 "*//2 _bd [*//2 _mt _cp] - **2 _hc"
  &* pe "*2 g1 g0.8"
v3 $ meta (pt "fa2,ea1%4 fa2 | /2 id shh]")
  (pe0 "g0.8 | <1%2 *2 [*//2 _can,1:2 [*//2 _hc,s1 1s2]] _mt,s1.2  \
  \ | pan0 [*2 pan1 pan-1] pan1")
