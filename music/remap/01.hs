cps 2.5
scales = slow 6 $ ps "maj dim 2dor 3dim 4phr -3aol"
sy = pe "_sy,sus1,f220"
onSy deg_epic = (syParams $ scales <*> deg_epic &* sy)
e = pd "0 0 1 0 2 0"
e0 = pd0 "0 0 1 0 2 0"
deg_epic = remapPd deg_p <$> e0
doublesToSpeeds = (M.!) $ M.fromList $ zip [0..] [id,fast 3,fast 2]
speed_epic = doublesToSpeeds <$> e

ep = slow 1.5 ( (meta (slow 2 speed_epic) $ pe0 "_bd _sn")
                +| pe0 "<1%4 //2 _ho _hc")
  +| onSy (remapPd deg_p <$> e0)
     &*   (cat $ replicate 5 (dsh $ 1/2) ++ [pe "f2"])
  +| onSy (remapPd deg_p . (+4) <$> e0)
  +| onSy (remapPd deg_p . (+9)
           <$< (early 3 $ cata 3 [fast 2,id,early (1/3),id])
           `meta` e0)
v0 ep
