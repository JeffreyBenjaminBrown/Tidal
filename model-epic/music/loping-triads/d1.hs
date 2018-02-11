scaleSeq = ps "/3 aol 5lyd3 8maj -1aug"
ut = cata 3 [id, down 3, down 5, id] -- unTranspose
  where down n = chVF deg_p $ \x -> x-3
tone = pe "_sy,,sus1,,f120"
bass parMap = tone &*            syParams <$< scaleSeq <*<        parMap
high parMap = tone &* pe "f2" &* syParams <$< scaleSeq <*< ut <*< parMap
arpeg = pe0 "[d0 | /2 d3 | /4 d11] d6 d9"
riseSeq = cata 3 $ fmap (chVF deg_p . (+) . (*2)) $ [-1..2] ++ reverse [0..3]
pedaledRises = (period .~ Just 2 $ riseSeq <*< arpeg)
  +- (fast 3 $ period .~ Just 3 $ riseSeq <*< arpeg)

lastEarly = pt "/3 id id ea1%3"
fastRevShift = pt "/2 rev - *//2 ea1%6,fa2 la1%6"
pow3 = pe0 "*3 _bd _sn,:1 _hc" &* pe "s0.7"
wubs = pe0 "*3 [_bottle,g1.2 - *2 _casio,:2,g1]" &* pe "s1,:0 s1.3,:1 s0.7,:2"
