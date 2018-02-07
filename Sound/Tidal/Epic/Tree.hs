module Sound.Tidal.Epic.Tree where
import Data.Tree (Tree(..), unfoldTree)
import Sound.Tidal.Epic.Types.Reimports

data DurNode a = DurNode {
  trepicPre :: Dur -- ^ sum of earlier durations
  , trepicDur :: Dur -- ^ duration of this one
  , trepicPost :: Dur -- ^ sum of later durations
  , trepicLoad :: a } deriving (Show, Eq, Ord)
type DurTree a = Tree (DurNode a)

toTree :: [(Dur,a)] -> DurTree a
toTree = unfoldTree go where
  go :: [(Dur,a)] -> (DurNode a, [[(Dur,a)]])
  go [] = error "go only makes sense for nonempty lists."
  go x = let (pre,(t,a):post) =
               splitAt (floor $ fromIntegral (length x) / 2) x
             stripNulls :: [[a]] -> [[a]]
             stripNulls [] = []
             stripNulls (a:as) = let rest = stripNulls as
                                 in if null a then rest else a:rest 
         in ( DurNode { trepicPre  = sum $ map fst pre
                      , trepicDur = t
                      , trepicPost = sum $ map fst post
                      , trepicLoad = a }
            , stripNulls [pre, post] )

