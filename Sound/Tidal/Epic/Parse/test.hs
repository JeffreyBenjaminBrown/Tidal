dur = 1
soundMap = M.singleton sound_p $ VS "hatc"
speedMap = M.singleton speed_p $ VF 2
degMap = M.singleton deg_p $ VF 3
parseBitSet = S.fromList [ CmdDur dur
                         , CmdParamPersist soundMap
                         , CmdParamPersist degMap
                         , CmdParamOnce speedMap ]
seqBit = CmdBlock (Just dur) False (M.union soundMap degMap) speedMap
seqBit == toCmdBlock parseBitSet
