melody = fast 3 $ p0 "t2 d0,,t1 d2,,t2 d1,,t1 d4"
scaleCyc = cata 1 [maj,maj6,lyd,aug]
sy = p "psy f440 sus0.3"
v0 $ sy &* syParams (scaleCyc <*> melody)
v1 $ sy &* syParams (scaleCyc <*> (late (1/2) $ mdeg (+2) melody))
v2 $ sy &* qf 0.5 &* syParams (scaleCyc <*> (fast 3 $ late (1/3) $ mdeg (+2) melody))

v3 $ fast 3 $ p0 "phatc,,pkick,,t1%2 psnare,,psnare"
