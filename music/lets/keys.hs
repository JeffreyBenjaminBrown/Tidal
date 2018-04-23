keys pat = pe "_sy,sus1,f220"
  &* remapPd qf_p
  <$< (\x->2**((x+4)/12)) -- The +4 puts the song in Db
  <$< pat

keysVerse = (keys $ slow 8 $ pd 
  "[0 | 4 | 7 | 11] [-3 | 0 | 4 | 7] [-7 | -3 | 0 | 4] [-4 | 0 | 3 | 7]")
  &* pe0 "<1%2 s1"
keysBridge = (keys $ slow 2 $ 
  pd "[4 | 7 | 11 | 14] [2 | 5 | 9 | 12] [0 | 4 | 7 | 11] [-1 | 2 | 5 | 8]"
  +- rep 4 (pd "[-3 | 0 | 4 | 11]"))
  &* pe0 "<1%2 s1"
keysChor = (keys $ slow 8 $ 
            pd "[2 | 5 | 9 | 12] [4 | 7 | 11 | 14] [2 | 5 | 9 | 12]")
  &* pe0 "<1%2 s1"
keysHappy = (keys $ slow 2 $ 
            pd " [0 | 5 | 9  | 16]  [4 | 7 | 11 | 14] \
               \ [4 | 9 | 12 | 16]  [2 | 7 | 11 | 19]")
  &* pe0 "<1%2 s1"
keysWeird = (keys $ slow 4 $ 
            pd " [2 | 5 | 9 | 12]  [2 | 5 | 9 | 12]  \
               \ [3 | 7 | 10 | 14] [2 | 5 | 9 | 12]  \
               \ [3 | 7 | 10 | 14] [3 | 7 | 10 | 14] ")
  &* pe0 "<1%2 s1"
