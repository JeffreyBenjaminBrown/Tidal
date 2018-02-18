module Sound.Tidal.Vector.DurVec where

import           Control.Lens
import           Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector.Algorithms.Intro (sortBy)
import           Data.Vector.Mutable
import           Data.Vector.Algorithms.Search (binarySearchP)

import Sound.Tidal.Vector.Types.Reimports
import Sound.Tidal.Vector.Types
import Sound.Tidal.Vector.VecEv


end :: DurVec a -> Time
end dv = if V.null v then 0 else veStart l + veDuration l
  where v = _dvPayload dv
        l = V.last v

-- todo ? maybe this should operate on Vectors, not DurVecs
sortDurVec' :: DurVec a -> DurVec a
sortDurVec' = dvPayload %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare a a' where
                         a  = (veStart ve , veDuration ve )
                         a' = (veStart ve', veDuration ve')
                   sortBy compare' v'
                   V.freeze v'

-- | Indices for an arc. ASSUMES the input DurVec is sorted.
arc :: Arc -> DurVec a -> [(Arc,a)]
arc (s,e) dv =
  let v = _dvPayload dv
      (si,ei) = runST $ do v' <- V.thaw v
                           first <- binarySearchP (overlapsEv (s,end dv) ) v'
                           last <-  binarySearchP (overlapsEv (0,e)      ) v'
                           return (first,last)
      subVector = V.slice si (ei - si + 1) v
      subVector' = V.map (clip (s,e)) subVector
  in map (\ve -> (support ve, vePayload ve)) $ V.toList subVector'

silence :: DurVec ParamMap
silence = DurVec { _dvPeriod = 1, _dvPayload = mempty }
