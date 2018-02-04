ep = pe0 "d0 d7 d6 d4 d6 d4 d3 d2" -- RATM
scales = ps "dor"
sy = pe "_sy,sus1,f440"
notSwing = cata 1 [fast 2, ((&*) shh)]

v0 $ syFreq <$< scales <*< meta notSwing ep &* sy
