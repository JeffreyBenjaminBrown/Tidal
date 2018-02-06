-- shortly into d1, drums need to change

:. d0
v9 $ pow3 +| wubs

-- start before _can
v0 $ bass $ early 0 arpeg &+ pe "d0 | *//3 d4 _ _"

v1 $ high $ meta lastEarly $ chVF deg_p (+2) <$> pedaledRises

v9 shh -- only two bars (half a chord cycle)

v9 $ meta (rev fastRevShift) (rev pow3)
v2 $ high $ meta (pt "/4 id ea1%3 rev,fa2") pedaledRises
v8 $ meta fastRevShift wubs

mapM_ ($ shh) [v0,v1,v2,v9]
:. d1

v9 $ pow3 +| wubs

v0 $ bass $ early 0 arpeg &+ pe "d0 | *//3 d4 _ _" -- start before _can
-- todo/compose : add something ambient here, starts with bass

v1 $ high $ meta lastEarly $ chVF deg_p (+2) <$> pedaledRises -- hurry

v9 $ meta (rev fastRevShift) (rev pow3)
v2 $ high $ meta (pt "/4 id ea1%3 rev,fa2") pedaledRises

v8 $ meta fastRevShift wubs
v7 $ pe "g1.2" &* (pe0 "/3 _clubkick" +| meta (pt "/3 ea1 la1") (pe0 "[_ade,:7,s8 | s9] - /2 _"))

mapM_ ($ shh) [v9,v0,v1,v2]
:. d0
v9 $ pow3 +| wubs

v7 shh
v0 $ bass $ early 0 arpeg &+ pe "d0 | *//3 d4 _ _"
v1 $ high $ meta lastEarly $ chVF deg_p (+2) <$> pedaledRises

v9 $ meta (rev fastRevShift) (rev pow3)
v2 $ high $ meta (pt "/4 id ea1%3 rev,fa2") pedaledRises

v8 $ meta fastRevShift wubs

mapM_ ($ shh) [v8,v9]
