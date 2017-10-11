{-# LANGUAGE OverloadedStrings #-}
module TestHasTextInternal where

import           Debug.Trace
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import qualified Data.HashMap.Strict                 as HS
import qualified Data.IntMap                         as IntMap
import           Data.Monoid
import           Test.Tasty.HUnit
import           TestData
import           WordEmbedding.HasText.Internal.Type
import           WordEmbedding.HasText.Internal.Strict.HasText

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

testUnsafeWindowRange :: Assertion
testUnsafeWindowRange = do
  assertEqual "winRange 5 and targetIdx 0 is" fromMinus5To0 winRange5TargetIdx0
  assertEqual "winRange 0 and targetIdx 0 is" fromMinus5ToMinus5 winRange0TargetIdx0
  assertEqual "winRange 5 and targetIdx 5 is" windowFive winRange5TargetIdx5
  assertEqual "winRange 0 and targetIdx 5 is" from0To0 winRange0TargetIdx5
  assertEqual "winRange 5 and targetIdx 10 is" from0ToPlus5 winRange5TargetIdx10
  assertEqual "winRange 0 and targetIdx 10 is" fromPlus5ToPlus5 winRange0TargetIdx10
  where
    winRange5TargetIdx0 = unsafeWindowPrim 5 sourceData 0
    winRange0TargetIdx0 = unsafeWindowPrim 0 sourceData 0
    winRange5TargetIdx5 = unsafeWindowPrim 5 sourceData 5
    winRange0TargetIdx5 = unsafeWindowPrim 0 sourceData 5
    winRange5TargetIdx10 = unsafeWindowPrim 5 sourceData 10
    winRange0TargetIdx10 = unsafeWindowPrim 0 sourceData 10
    fromMinus5To0, fromMinus5ToMinus5, from0To0, from0ToPlus5, fromPlus5ToPlus5, windowFive, sourceData :: V.Vector T.Text
    fromMinus5To0      = V.fromList ["-4", "-3", "-2", "-1", "0"]
    fromMinus5ToMinus5 = V.fromList []
    windowFive         = V.fromList ["-5", "-4", "-3", "-2", "-1", "+1", "+2", "+3", "+4", "+5"]
    from0To0           = V.fromList []
    from0ToPlus5       = V.fromList ["0", "+1", "+2", "+3", "+4"]
    fromPlus5ToPlus5   = V.fromList []
    sourceData         = V.fromList ["-5", "-4", "-3", "-2", "-1", "0", "+1", "+2", "+3", "+4", "+5"]
