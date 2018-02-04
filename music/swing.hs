line1 = pe0 "d0 d7 d6 d4 d6 d4 d3 d2" -- RATM
line2 = pe0 "d0 d7 d6 d4 d6 d4 - /2 d9" -- RATM
ep = line1 +- line2
scales = ps "dor"
sy = pe "_sy,sus1,f440"

v0 $ fast 4 $ syFreq <$< scales <*< swing 2 (4%3) (late 1 ep) &* sy
