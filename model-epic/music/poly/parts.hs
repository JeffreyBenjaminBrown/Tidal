drums =    toDrums <$< seq0
biggerDrums = toBiggerDrums <$< seq0
kick = toKickOnZero <$< seq0
chiff = fast 4     $ (period .~ Just 3 $ toChiff <$< seq0)      +- dsh 2
toms = early (1/4) $ (period .~ Just 4 $ toToms  <$<
                     meta (cata 1 [id, fast 2]) (early 1 seq0)) +- dsh 1

bass = onSyBass $ meta (cata 1 [early (1/2), id]) melody
          &+ transposes &* pe "g0.4"
mid = onSy (pe "g0.3"
            &* (fast 2 $ chVF deg_p (+9) <$< melody)
            &+ transposes &* pe "/2 [f1 | _ [f0.5,g0.7 | f1.5]]")
high = onSy $ (late 0.5 $ chVF deg_p (+18) <$< melody)
               &+ transposes &* pe "g0.15 | pa2.25"
               &* pe "fa0.02,ff0.003"
