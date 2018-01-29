stut n x = x +| late n x &* pe "g0.8"
stutHalf n = cata 1 [id, stut n, id, stut $ -n]
halfOn n = cata n [id, (&* shh)]
halfSlow n = cata n [id, slow n]
halfEarly n = cata n [id, early n]
v1 $ meta (stutHalf $ 1/4) $ pe0 "_bd _sn:0"
v2 $ meta (halfOn 4) $ pe0 "<1%2 _cp"
v3 $ meta (meta (halfEarly 1) $ halfSlow 8)
  $ meta (early 1 $ stutHalf $ 1/8)
  $ pe0 "*4 _hc,,g0.9" &* pe "*3%2 g0.8 g0.9 g1"

v4 $ syFreq $ syHeavyBass
  &* (meta (stutHalf $ 1/4) $ pe0 "t1%4,,f1 f1 f1 _ _ f1.5 _ g0.5")
  &* pe "/8 f1 f1.15"

v5 $ fast 2 ( syFreq $ syHeavyBass
              &* ( meta (stutHalf $ 1/4)
                  $ meta (slow 4 $ halfEarly $ 1/2)
                  $ meta (early 0.5 $ slow 8 $ halfSlow 2)
                  $ pe0 "t1%4,,f1 f1 f1 _ _ f1.5 _ g0.5") )
  &* pe "/8 f1,g0.9 f1.15"
  &* pe "/2 f6 f7" &* pe "f1 | /4 _ fr7%4,g0.9"

v6 $ pe0 "<1 /8 _birds,:0 :1 :2 :3"
  +| pe0 "<4 /16 _battles:0 :1" &* pe "s0.33,g0.85"
