hush
cps 1.5

sync2 = pt "ea1%4,fa2 ea1%2"

v0 $ pe0 "_bd _sn - <1%2 [*//2 _ [*//2 _mt _ht]]"
  +| meta (slow 3 sync2) (pe0 "_hc,g0.9 _hh,g1.2 _")
  +| pe "_bottle" &* pe0 "*4 s1,1:1 [*//2 s1 _] [*//3 s1 _ _]"
                  &* pe "/2 s0.7,1g0.8 s1"

v0 $  (fast 2 $ (period .~ Just 3 $ pe0 "_bd _sn")
       +- meta (pt "//2 fa2 ea1%2") (pe0 "_hc,g0.9 _hh,g1.2") )
  +| pe "_bottle"
    &* pe0 "*4 s1,1:1 [*//2 s1 _] [*//3 s1 _ _]"
    &* pe "/2 s0.7,1g0.8 s1"

v0 $ pe0 "_bd _sn"
  +| meta (slow 2 sync2) (pe0 "_hc,g0.9 _hh,g1.2")
  +| pe "_bottle _casio,:2 "
    &* pe0 "*4 s1,1:1 [*//2 s1 _] [*//3 s1 _ _]"
    &* pe "/2 s0.7,1g0.8 s1"
line = pd0 "/**3 [0|2 1|4] [0|3|6] [*//2 _ [*//2 *4 0 1 2 3 3 2 1 0]]"
line = pd0 "/**2 [0|2|4] [0|3|6] [*//2 _ [*//2 *4 0 1 2 3 3 2 1 0]]"
tone = pe "_sy,sus2,f220"
v9 $ slow 2 $ tone &*
  syParams <$< ps "/4 aol7 1lyd 4lyd [*//2 2loc 7mix2]"
  <*< meta (slow 2 sync2)
     (remapPd deg_p <$< (line +| (+ (9)) <$< line))
