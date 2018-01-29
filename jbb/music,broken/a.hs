melody = fast 3 $ p0 "t2 d0,,t1 d2,,t2 d1,,t1 d4"
scaleCyc = cata 1 [maj,maj6,lyd,aug]
sy = p "psy f440 sus0.3"
v0 $ sy &* syFreq (scaleCyc <*> melody)
v1 $ sy &* syFreq (scaleCyc <*> (late (1/2) $ mdeg (+2) melody))
v2 $ sy &* qf 0.5 &* syFreq (scaleCyc <*> (fast 3 $ late (1/3) $ mdeg (+2) melody))

v3 $ fast 3 $ p0 "phatc,,pkick,,t1%2 psnare,,psnare"
