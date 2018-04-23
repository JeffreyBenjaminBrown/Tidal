-- All My Life, by (the?) Foo Fighters

pitchPattern pat = remapPd qf_p <$< (\x->2**(x/12)) <$< pat
guit pat = pe "_sy,sus1,f110,g0.8" &* pitchPattern pat
vox pat =  pe "_sy,sus1,f220"      &* pitchPattern pat

v1 $ pe "up0 | up7" &* (guit $ pd0 "*4 0 0 0 _")
v2 $ vox $ pd0 "*2 3 3 3 [**2 3 3 3] [*2 _ 3] _       \
               \ 3 [*2 3 3 3 3 _ 3 3 3 3 _ 3 _ 3 _]   \
               \ 3 [*2 3 3 3 3 _ 3 3 3 3 _ 3 _ _ _]   \
               \ 3 [*2 3 3 3 3 _ 0 0 0 0 0 0 -2 _ _] "
v3 $ pe0 "_bd _sn"
