module WordEmbedding.HasText.Internal.SpVector
  ( norm2
  , scale
  , plus
  , dot
  , unitVector
  , cosSim
  )
where

import qualified Data.IntMap                                   as IntMap
import           WordEmbedding.HasText.Internal.Type           (SpVector)
import           Data.Monoid

norm2 :: SpVector -> Double
norm2 v = sqrt . getSum . foldMap (Sum . (** 2)) $ v

scale :: Double -> SpVector -> SpVector
scale coeff v = IntMap.map (coeff *) v

plus :: SpVector -> SpVector -> SpVector
plus = IntMap.unionWith (+)

dot :: SpVector -> SpVector -> Double
dot v1 v2 = sum $ IntMap.unionWith (*) v1 v2

unitVector :: SpVector -> SpVector
unitVector v = scale (1 / norm2 v) v

cosSim :: SpVector -> SpVector -> Double
cosSim nume deno = dot (unitVector nume) (unitVector deno)
