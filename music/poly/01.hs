(seq,seq0) = pd2 "[0 - *//2 3 2] - 0 1 1 0"
toBigDrums = defaultMap $ pel "_cr _bd _cp _sn _cr"
toToms = remapPs sound_p . defaultMap ["tabla", "ht","mt","lt"]
toChiff = remapPs sound_p . defaultMap ["bin", "bend","chink","ho"]
ep = dsh 1
  +|                                    toBigDrums <$< slow 2 seq0
  +| (early (1/2) $ (period .~ Just 4 $ toToms  <$<           seq0) +- dsh 1)
  +| (fast 4      $ (period .~ Just 3 $ toChiff <$<           seq0) +- dsh 2)
v0 ep
