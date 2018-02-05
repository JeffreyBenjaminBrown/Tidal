scaleSeq = ps "/3 maj 5lyd 9aol -1loc"
ut = cata 3 [id, down 3, down 5, id] -- unTranspose: remove scale artifact
  where down n = chVF deg_p $ \x -> x-3
tone = pe "_sy,,sus1,,f150"
bass parMap = tone &*            syParams <$< scaleSeq <*<        parMap
high parMap = tone &* pe "f2" &* syParams <$< scaleSeq <*< ut <*< parMap
arpeg = pe0 "d0 d2 d4"
riseSeq = cata 3 $ fmap (chVF deg_p . (+)       ) $ [-1..2] ++ reverse [0..3]
pedaledRises = (period .~ Just 2 $ riseSeq <*< arpeg)
  +- (fast 3 $ period .~ Just 3 $ riseSeq <*< arpeg)

lastEarly = pt "/3 id id ea1%3"
fastRevShift = pt "/2 rev - *//2 ea1%6,fa2 la1%6"
pow3 = pe0 "*3 _bd _sn _hc"
wubs = pe0 "*3 [_can,g1.2 - *2 _ht,g1]" &* pe "s1,:0 s1.2,:1 s1.4,:2"
