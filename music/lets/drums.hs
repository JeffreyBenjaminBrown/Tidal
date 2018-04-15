drumIntro = pe0
 " _hh [_hh | _lt] [*2 _hh _bd] _hh  - _hh [_hh | _mt ] [*2 _lt _bd _ _sn] \
 \ _hh [_hh | _lt] [*2 _hh _bd] _hh  - _hh [_hh | _cow] [*2 _lt _bd _ _sn] "

toms = fast 2 $ pe0 "_ [**2 _mt]" &* pe "g0.8"
bdsn = pe0 "_bd _sn [*2 _bd _bd,1s0.8,1g0.9] _sn"
bdsn2 = pe0 "_bd,1s0.8 | _sn,g0.9"

hat8  = pe0 ">1%2 [t7,_ - t1%2,_ho _hc]"
hat16 = pe0 ">1%2 [t15,_ - t1%2,_ho _hc]"
hat32 = pe0 ">1%2 [t31,_ - t1%2,_ho _hc]"
