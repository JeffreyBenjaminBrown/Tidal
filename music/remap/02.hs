cps 2
scale = ps "/32 [maj - *2 7phr3 - *4 5dor4 -2mix]"
sy = pe "_sy,sus1,f200"
onSy degPat = sy &* syParams (scale <*> degPat)
(e,e0) = pd2 "2 0 3 0 2 0 - //2 *4 2 3 2 1 - 0"
drums = pe "g0.9" &* remapPs sound_p <$< toDrum <$< e0 where
  toDrum = (M.!) $ M.fromList $ zip [0..] ["hc", "sn", "bd", "can"]
degs = remapPd deg_p <$< e0
changes = toChange <$< e where
  toChange = (M.!) $ M.fromList
    $ zip [0..] [id, early (1/2), dense 2, early (1/2) . dense 2]

v0 $ onSy (remapPd deg_p <$< e0)
  +| onSy (remapPd deg_p
            <$< cata 4 [(+9),(+11),(+13),(+14)]
            <*< slow 4 changes
            `meta` cata 8 [id, sparse 2, id, dense 2, dense 4]
            `meta` e0)
     &* pe "g0.9"
  +| meta (slow 2 changes) drums
  +| (meta (cata 32 [late $ 1/4, late $ 1/2])
      $ meta (slow 4 changes) drums
       &* pe "pan0,g0.9 pan1,g0.8")
