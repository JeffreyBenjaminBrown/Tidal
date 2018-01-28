f n = cata 1 [id, late n]
v1 $ pe0 "_bd | [*//2 _cr,,s0.75 s1 - /7 _]"
v2 $ meta (f (1/4) +| (early 1 $ f $ 1/8)) $ pe0 "<1%2 _sn"
v2 $ meta (f $ 1/4) $ pe0 "<1%2 _sn"
v3 $ fast 2 $ meta (f $ 1/4) $ pe0 "<1%2 _hc - *//4 _hc _hc _ho _hc"

v4 $ pe "_sy,,sus1,,f80,,fa1.3,,ff2"
  &* meta
      ((breathe 4 $ f $ 1/8) +| loopa 1 id)
      (pe0 "**2 f1 - >1%8 **2 f1.5")
  &* (fast 2 $ pe "f1 - *2 //4 [f1.2 f0.95]")
  &* (slow 4 $ pe "f1 - *3 f1.5 f2")

v5 $ slow 2 $ (syParams $ speed $ foldr1 (+|) [1,3/4,7/5])
  &* (meta (f $ 1/4) $ pe0 "*2 _sy,,g0.85,,sus3,,f440" )
  &* fast 2 ( (period .~ Just 2 $ pe "f1 - *3 f1.5 - f1.75")
              +- (pe "f7 | f3") )
  &* ( (period .~ Just 2 $ (slow 3 $ pe "f1 f1.1 f0.8"))
      +- pe "f0.2")
