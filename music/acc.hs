mel = fast 3 $ pe0 "d0 d2 - **2 d4"
chords = ps "//2 lyd phr -2lyd -2phr"
v1 $ slow (3%2) $ chords <*> (chVF deg_p (+2)   <$> (mel &+ pe "_psr,,s0.5"))
v2 $ slow (3%2) $ chords <*> (early (1/3) $ fast 2 $ mel &+ pe "_psr")
v3 $ slow (3%2) $ chords <*> (late (1/4) $ (cata 1 $ chVF deg_p <$> [(+1),(+2)]) <*> mel &+ pe "_psr")
v3 $ slow (3%2) $ chords <*> (late (1/4) $ chVF deg_p (+2) <$> mel &+ pe "_psr")



v1 $ pe "_sy,,sus0.7,,f80,,g1.1" &* syParams (ps "maj 5dor 7phr -2lyd" <*> pe0 "*4 [d0 d2 d4 - *2 //3 d5 d7]")

v1 $ (syParams $ ps "//4 maj 6dor" <*> pe0 "**2 d0 - *2 d2 d3 - **8%3 d4") &* sustain 2 &* qfa 1 &* qf 220 &* sound (ever "sy")
v2 $ pe0 "*2 _bd _sn +| <1%8 *8 //2 [_ho,,g0.7 - [//2 _ho _hc]]"
