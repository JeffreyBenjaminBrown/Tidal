import Control.Monad.ST
import Data.Vector
import Data.Vector.Algorithms.Intro (sort)
import Data.Vector.Mutable

x = fromList [1,4,2] :: Vector Int

verboseCopy :: Vector Int
verboseCopy = runST $ do
  v <- thaw x
  sort v
  freeze v
