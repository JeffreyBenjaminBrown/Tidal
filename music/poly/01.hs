(seq,seq0) = pd2 "0 - *//2 3 2 - 0 - *//2 [**2 1 - 1] - *2 1 0"
toBigDrums = defaultMap $ pel "_cr _bd _cp _sn _cr,g0.9"
toToms = remapPs sound_p . defaultMap ["tabla", "ht","mt","lt"]
toChiff = remapPs sound_p . defaultMap ["bin", "bend","chink","ho"]
drums = dsh 1 -- written this way, any component below is easily commented out
  +|                                    toBigDrums <$< slow 2 seq0
  +| (early (1/2) $ (period .~ Just 4 $ toToms     <$<        seq0) +- dsh 1)
  +| (fast 4      $ (period .~ Just 3 $ toChiff    <$<        seq0) +- dsh 2)

scales = ps "/5 maj 5maj6"
onSy deg_epic = (syParams $ scales <*> deg_epic &* pe "_sy,sus1,f220")
melody = remapPd deg_p <$< (*2) <$< seq0
transposes = slow 10 $ remapPd deg_p <$< (*(-2)) <$< seq

v0 $ drums
  +| (onSy $ melody &+ transposes)
  +| onSy (pe "g0.3"
           &* (fast 2 $ chVF deg_p (+9) <$< melody)
           &+ transposes)
  +| onSy ((late 0.5 $ chVF deg_p (+18) <$< melody)
           &+ transposes &* pe "g0.15 | pa2.25")
