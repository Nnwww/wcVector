module WordEmbedding.HasText.Internal.SpVector
  ( norm2
  , scale
  , plus
  , dot
  , unitVector
  , cosSim
  , map
  , fromList
  , limXLogOneXth
  )
where

import           Prelude                             hiding (map)
import qualified Data.IntMap                         as IntMap
import           WordEmbedding.HasText.Internal.Type (SpVector)
import           Data.Monoid

norm2 :: SpVector -> Double
norm2 = sqrt . getSum . foldMap (Sum . (** 2))

scale :: Double -> SpVector -> SpVector
scale coeff v = IntMap.map (coeff *) v

-- Note: addition of each element of IntMap is unionWith (+).
-- On the other hand, multiplication of each element of IntMap is intersectionWith (*).
plus :: SpVector -> SpVector -> SpVector
plus = IntMap.unionWith (+)

dot :: SpVector -> SpVector -> Double
dot v1 v2 = sum $ IntMap.intersectionWith (*) v1 v2

unitVector :: SpVector -> SpVector
unitVector v = scale (1 / norm2 v) v

cosSim :: SpVector -> SpVector -> Double
cosSim nume deno = dot (unitVector nume) (unitVector deno)

map :: (Double -> Double) -> SpVector -> SpVector
map = IntMap.map

fromList :: [(Int, Double)] -> SpVector
fromList = IntMap.fromList

limXLogOneXth :: Double -> Double
limXLogOneXth x = if x == 0 then 0 else x * log (1 / x)
