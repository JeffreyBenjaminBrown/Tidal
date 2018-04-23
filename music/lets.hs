-- Let's Stay Together, by Al Greene

import Control.Concurrent (threadDelay)

:. lets/drums
:. lets/bass
:. lets/keys

cps $ 5/3 -- the song's a little faster than this, but this is easier

start = do cps (-1)
           threadDelay 100000
           cps (5/3)

-- skipping: intro  = drumIntro +| ...
verse  = bdsn  +| toms +| hat32 +| bassVerse  +| keysVerse
bridge = bdsn2 +| toms +| hat16 +| bassBridge +| keysBridge
chor   = bdsn  +| toms +| hat8  +| bassChor   +| keysChor
happy  = bdsn2 +| toms +| hat8  +| bassHappy  +| keysHappy
weird  = bdsn  +| toms +| bassWeird +| keysWeird

v1 $ pe0 "_hh,g1.2 _hh"
  +- verse +- bridge +- verse +- bridge +- chor +- happy +- weird
  +- happy +- verse +- bridge +- chor +- happy +- chor  +- happy

start
