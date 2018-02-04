(seq,seq0) = pd2 "0 - *//2 3 2 - 0 - *//2 [**2 1 - 1] - *2 1 0"
toDrums = defaultMap $ pel "_cr _bd,:0 _sn _cp _cow"
toBiggerDrums = defaultMap $ pel "_cr _clubkick,:0 _sn _cp _bleep"
toKickOnZero = defaultMap $ pel "_ _clubkick,:0"
toToms = remapPs sound_p . defaultMap ["tabla", "ht","mt","lt"]
toChiff = remapPs sound_p . defaultMap ["bin", "bend","chink","ho"]

scales = ps "/5 maj 5maj6"
onSy deg_epic = syParams <$< scales <*< deg_epic &* pe "_sy,sus1,f220"
  &* fast 4 (pe "g1 g0.9fa0")
onSyBass deg_epic = syParams <$< scales <*< deg_epic &* syFuzzBass
melody = remapPd deg_p <$< (*2) <$< seq0
transposes = slow 10 $ remapPd deg_p <$< (*(-2)) <$< seq
