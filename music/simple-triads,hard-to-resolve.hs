tone = pe "_sy,,sus1,,f150"
scaleSeq = ps "/3 maj 5lyd 9aol -1loc"
noTranspose = cata 3 [id, f 3, f 5, id] where f n = chVF deg_p $ \x -> x-3
arpeg = pe0 "d0 d2 d4"
riseSeq = cata 3 $ fmap (chVF deg_p . (+)) $ [-1..2] ++ reverse [0..3]
pedaledRises = (period .~ Just 2 $ riseSeq <*< arpeg)
  +- (fast 3 $ period .~ Just 3 $ riseSeq <*< arpeg)

v1 $ syParams $ tone &* pe "s2" &* scaleSeq <*< noTranspose <*< pedaledRises
v2 $ syParams $ tone &* pe "s2" &* scaleSeq <*< noTranspose
  <*< ( meta (cata 3 [id,id,early $ 1/3])
        $ chVF deg_p (+2) <$> pedaledRises )
v3 $ syParams $ tone &* scaleSeq <*> (early 0 arpeg)
