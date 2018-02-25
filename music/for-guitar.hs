pat = fast 8 $ pe0 "/3 _sy - /2 _sy - /3 _sy"
f pat = pat +- early (1/8) pat
syt = syHollowBass' &* pe "f107"
v0 $ syt &* meta (slow 8 $ pt "/2 id - shh")
                 (f $ f pat)
  &* pe "/8 fr1 fr4%3,g0.9" -- pitch
v1 $ pe0 "_bd _sn"
