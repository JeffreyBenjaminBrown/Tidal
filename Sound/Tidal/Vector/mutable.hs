import Control.Monad.ST
import Data.Vector
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Mutable
import Data.Ord (Ordering(..))

x = fromList [(1,101),(4,99),(2,100)] :: Vector (Int,Int)
compare' e f = compare (fst e) (fst f)

xSorted :: Vector (Int,Int)
xSorted = runST $ do
  v <- thaw x
  sortBy compare' v
  freeze v
