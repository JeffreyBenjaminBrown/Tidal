cps 1.2
scales = slow 8 $ ps "dor 1lyd 5dor +- *2 //2 1lyd -2phr"
f n = cata 1 [early $ 1/n, id]
f'  = early (1/4) $ cata 1 [id,id,id, dense 2]
g = cata 1 $ replicate 7 id ++ [dense 2]
g' = cata 1 [(&* shh), dense 2, (&* shh), early $ 1/4]
beat = pe0 "_bd" +| (meta (f 4) $ early (1/4) $ pe0 "_sn")
chiff = (meta (early 2 $ slow 4 $ f 2)
      $ fast 2 $ meta g' $ pe0 ">1%2 *2 //2 _ho _hc")
bass = pe "_sy,,sus1,,f80,,g1.1 +| f120,,g0.95 f160,,g0.85"
bassline = pe0 "t3%4,,d0 t1%4,,d6 +- *2 //2 [**2 t1,,d4 +- d4]"
sax = pe "_sy,,sus1,,f320,,fa2 +| f1500,,g0.7"
saxline = pe0 "d0 +- *2 d2 d4 +- d1 _"
can = pe "_bottle,,s0.6,,g0.9 _can,,g1.2,,s0.8"
canline = pe0 "*4 s1 s1.2 s1.5"
meta (cata 1 [id,id,fast 2])


hush
scales = slow 8 $ ps "dor"
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)

v1 $ early (1/8) $ chiff

v3 $ early 1 $ bass &* syParams bassline

v3 shh
v5 shh

hush
v1 $ chiff +| beat
v3 $ early 1 $ bass &* syParams (scales <*> bassline)
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)

v5 $ meta (cata 1 [id,fast 2]) canline &* can

v5 $ early 1 $ sax &* (syParams $ meta g $ scales <*> late 1 saxline) &* pe "*3 f1.5 f0.5 f1.5 +- *2 //2 f0.5 f0.75"
v1 shh

v1 $ beat

v1 $ beat +| chiff

hush
scales = ps "dor"
v1 $ chiff +| beat
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)

hush
scales = slow 8 $ ps "dor +- *2 //2 [5dor -2dor]"
v1 $ pe0 "_bd _bd"
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)

scales = slow 8 $ ps "dor +- *2 //2 [5dor -2dor]"

hush
v1 $ beat
v2 $ early 1 $ bass &* (syParams $ scales <*> bassline)
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)
v1 $ beat +| chiff
v3 $ early 2 $ sax &* (syParams $ meta g $ scales <*> (chVF deg_p (+6) <$> fast 2 saxline)) &* pe "g0.9"
v5 $ meta (cata 1 [id,fast 2]) canline &* can
v6 $ early 1 $ sax &* (syParams $ meta g $ scales <*> late 1 saxline) &* pe "*3 f1.5 f2 f1.5 +- *2 //2 f1.5 f3" &* pe "ff0.5 ff2 ff5"

v3 shh
v1 beat

v5 shh
v6 shh
v6 $ early 1 $ sax &* (syParams $ meta g $ scales <*> late 1 saxline) &* pe "*3 f1.5 f2 f1.5 +- *2 //2 f1.5 f3" &* pe "ff0.5 ff2 ff5"

hush
v2 $ early 1 $ bass &* (syParams $ meta (f' +- loopa 4 id) $ scales <*> bassline)
v4 $ early 1 $ sax &* (syParams $ meta g $ scales <*> saxline)

v2 $ early 1 $ bass &* (syParams $ scales <*> bassline)
v6 $ early 1 $ sax &* (syParams $ meta g $ scales <*> late 1 saxline) &* pe "*3 f1.5 f2 f1.5 +- *2 //2 f1.5 f3" &* pe "ff0.5 ff2 ff5"
v5 $ slow 2 $ meta (cata 1 [id,fast 2]) canline &* can

v1 $ beat 

v3 $ early 2 $ sax &* (syParams $ meta g $ scales <*> (chVF deg_p (+6) <$> fast 2 saxline)) &* pe "g0.9,,fa0.02,,ff0.25"
v1 $ beat +| chiff

v1 $ early (1/8) $ chiff
v5 $ meta (cata 1 [id,fast 2]) canline &* can

v5 shh

v3 shh
v4 shh
v6 shh
v1 $ beat +| chiff

hush
