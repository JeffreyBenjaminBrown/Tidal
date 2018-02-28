pat1 = pe0 "*2 _bd _sn"
pat2 = pe0 "_hc"
aMap = M.fromList [("pat1",pat1),("pat2",pat2)]
anEpic = dj (pdjm "   /3 @pat1  \
                  \ - [(g0.9,s1.5),@pat1 | ea1%4,fa2,@pat2]") aMap
v0 $ anEpic
