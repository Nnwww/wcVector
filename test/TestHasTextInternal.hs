module TestHasTextInternal where

import qualified Data.HashMap.Strict                           as HS
import qualified Data.IntMap                                   as IntMap
import           Data.Monoid
import           Test.Tasty.HUnit
import           TestData
import           WordEmbedding.HasText.Internal.Type

testInitWVRefMakeOneHotVectors :: Assertion
testInitWVRefMakeOneHotVectors = assert (allExactlyOne . getSpVs <$> noFailWordVec)

getSpVs :: WordVec -> [SpVector]
getSpVs = map _wI . HS.elems

allExactlyOne :: [SpVector] -> Bool
allExactlyOne = getAll . foldMap (All . exactlyOne)
  where
    exactlyOne :: SpVector -> Bool
    exactlyOne spV =
      let listOfSpV = IntMap.elems spV in
        length listOfSpV == 1 && head listOfSpV == 1.0
