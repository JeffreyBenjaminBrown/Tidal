ep1 = pe0 "*2 _bd _sn"
ep2 = pe0 "_hc"
aMap = M.fromList [("ep1",ep1),("ep2",ep2)]
anEpic = dj (pdjm "   /3 @ep1  \
                  \ - [@ep1 | ea1%4,fa2,(g1.2),@ep2]") aMap

v0 $ anEpic
