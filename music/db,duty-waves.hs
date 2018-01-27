stut n x = x +| late n x &* pe "g0.8"
stutHalf n = cata 1 [id, stut n, id, stut $ -n]
halfOn n = cata n [id, (&* shh)]
halfSlow n = cata n [id, slow n]
halfEarly n = cata n [id, early n]
v1 $ meta (stutHalf $ 1/4) $ pe0 "_bd _sn"
v2 $ meta (halfOn 4) $ pe0 "<1%2 _cp"
v3 $ meta (meta (halfEarly 1) $ halfSlow 8)
  $ meta (early 1 $ stutHalf $ 1/8)
  $ pe0 "*4 _hc,,g0.9" &* pe "*3%2 g0.8 g0.9 g1"

tone = pe "_sy,,sus1,,f150,,fa1,,ff0.25,,g0.5"
v4 $ syParams $ tone 
  &* (meta (stutHalf $ 1/4) $ pe0 "t1%4,,f1 f1 f1 _ _ f1.5 _ g0.5")
  &* pe "/8 f1 f1.2"
