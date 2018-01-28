cps 2.5
scales = slow 6 $ ps "maj dim 2dor 3dim 4phr -3aol"
sy = pe "_sy,sus1,f220"
onSy deg_epic = (syParams $ scales <*> deg_epic &* sy)
e = pd "0 0 1 0 2 0"
e0 = pd0 "0 0 1 0 2 0"
deg_epic = remapPd deg_p <$> e0
doublesToSpeeds = M.fromList $ zip [0..] [id,fast 3,fast 2]
speed_epic = (M.!) doublesToSpeeds <$> e

v0 $ slow 1.5 ( (meta (slow 2 speed_epic) $ pe0 "_bd _sn")
                +| pe0 "<1%4 //2 _hc _ho")
  +| onSy (remapPd deg_p <$> e0)
      &*   (cat $ replicate 5 (dsh $ 1/2) ++ [pe "f2"])
  +| onSy (remapPd deg_p . (+4) <$> e0)
  +| onSy (remapPd deg_p . (+9)
           <$< cata 3 [fast 2,id,id,fast 2]
           `meta` e0)
