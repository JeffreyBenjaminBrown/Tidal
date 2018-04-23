
hush
v0 $ meta (pt "/3 [/7 id] shh")
  $ pe0 "_bd _sn - <1%2 [*//2 _ [*//2 _mt _ht,s1.3]]"
v1 $ meta (slow 3 $ pt "ea1%4,fa2 ea1%2") (pe0 "_hc,g0.9 _hh,g1.2 _")
v2 $ meta (pt "/24 id shh")
  $ pe "_bottle" &* pe0 "*4 s1,1:1 [*//2 s1 _] [*//3 s1 _ _]"
                  &* pe "/2 s0.7,1g0.8 s1" -- gain b/c slow one is loud

hush
v0 $ fast 2 $ meta (pt "/8 id rev")
  $ meta (pt ">1%4 id - *//2 la1%2 id") $ pe0 "_bd _sn"
v1 $ meta (ptm "*3%2 (g1) (s1.4,g0.8)")
  $ meta (ptm "/8 [*2 shh la1%4] fa2 id [fa2 | ea1%4]")
  $ meta (pt "id - *//2 ea1%2 id") $ pe0 "<1%4 *2 _mt,g0.8 - *2 _hc"

p1 = meta (ptm "/4 <1%2 [ea1%4 - [fa2 (s1.5)]]") $ pe0 "[*//2 _bd _cow] _sn"
p2 = early (1/4) $ meta (pt "/4 sl2 sl2,rev")
   $ pe "_lt _hc" &* pe0 "*4 s1 [*//2 s1 _] [*//3 s1 _ _]"
p3 = pe "_bottle" &* pe0 "*4 s1,1:1 [*//2 s1 _] [*//3 s1 _ _]"
                  &* pe "/2 s0.7,1g0.8 s1" -- gain b/c slow one is loud
v0 $ p1 +| p2 +| p3
