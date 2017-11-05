module TestSpVector where

import           WordEmbedding.HasText.Internal.Type     (SpVector)
import           WordEmbedding.HasText.Internal.SpVector as SV
import           Test.Tasty.HUnit

eg1 :: SpVector
eg1 = SV.fromList [(1,1),(2,1),(3,1)]

testNorm2 :: Assertion
testNorm2 = assertBool "SpVector's norm2 is" (actual == sqrt3RoundDown10e5)
  where
    actual, sqrt3RoundDown10e5 :: Int
    actual = floor $ (SV.norm2 eg1) * (10**3)
    sqrt3RoundDown10e5 = floor $ (sqrt 3 :: Double) * (10**3)


testScale :: Assertion
testScale = assertEqual "SpVector's scale is" expected (SV.scale (-1) eg1)
  where
    expected = SV.fromList [(1,-1),(2,-1),(3,-1)]

testPlus :: Assertion
testPlus = assertEqual "SpVector's plus is" expected (SV.plus added eg1)
  where
    added = SV.fromList [(1,-1),(2,0),(3,1),(4,3)]
    expected = SV.fromList [(1,0),(2,1),(3,2),(4,3)]

testDot :: Assertion
testDot = assertEqual "SpVector's dot is " expected (SV.dot dotted eg1)
  where
    dotted = SV.fromList [(3,1),(4,1)]
    expected = 1
