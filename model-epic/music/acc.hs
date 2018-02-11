mel = fast 3 $ pe0 "d0 d2 - **2 d4"
chords = ps "//2 lyd phr -2lyd -2phr"
v1 $ slow (3%2) $ chords <*> (chVF deg_p (+2)   <$> (mel &+ pe "_psr,,s0.5"))
v2 $ slow (3%2) $ chords <*> (early (1/3) $ fast 2 $ mel &+ pe "_psr")
v3 $ slow (3%2) $ chords <*> (late (1/4) $ (cata 1 $ chVF deg_p <$> [(+1),(+2)]) <*> mel &+ pe "_psr")
v3 $ slow (3%2) $ chords <*> (late (1/4) $ chVF deg_p (+2) <$> mel &+ pe "_psr")

v0 $ pe "_sy,,sus0.7,,f80,,g1.1" &* syParams <$< (ps "maj 5dor 7phr -2lyd" <*> pe0 "*4 [d0 d2 d4 - *2 //3 d5 d7]")

v1 $ (syParams <$< ps "//4 maj 6dor" <*< pe0 "**2 d0 - *2 d2 d3 - **8%3 d4") &* sustain 2 &* qfa 1 &* qf 220 &* sound (ever "sy")
v2 $ pe0 "*2 _bd _sn +| <1%8 *8 //2 [_ho,,g0.7 - [//2 _ho _hc]]"

v0 $ pe0 "*3 fr1%4 fr1%2 t1%2,f1.5 f1.2" &* pe "_sy,sus1,f440,fa0.02,ffabs4,ff0.004" &* pe "f1 | fr7%4 | fr8%5"

v0 $ fast 2 $ meta (pt "/2 fa2 - *//2 ea1%4,fa2 la1%4") $ pe0 "_bd _sn"
v0 $ fast 2 $ meta (pt "/2 fa2 - *//2 ea1%4,fa2 la1%4") $ pe0 "_bd - *2 _sn _hc"
v0 $ fast 2 $ meta (pt "/2 fa2 - *//2 ea1%4,fa2 la1%4") $ pe0 "*//2 _bd [_bd | *//2 _cp _ho] - *2 _sn _hc"
