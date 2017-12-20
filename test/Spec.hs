{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestDict
import           TestModel
import           TestData
import           TestHasText
import           TestHasTextInternal
import           TestSpVector

import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText
import           WordEmbedding.HasText.Internal.Type


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [ testCase "testaddEntryWithID add entries" $ testaddEntryWithID
            , testCase "wordsFromFile read a file" $ testReadCorrectlyWordsFromFile
            , testCase "(wordsFromFile addEntries) collect entries from file" $ testCollectFromFile
            , testCase "testInitFromFile is non zero" $ testInitFromFile
            , testCase "initWVRef make one-hot vectors" $ testInitWVRefMakeOneHotVectors
            , testCase "testUnsafeWindowRange" $ testUnsafeWindowRange
            , testCaseSteps "computeHidden" $ testComputeHidden
            , testCase "SpVector's norm2 is vaild" $ testNorm2
            , testCase "SpVector's scale is vaild" $ testScale
            , testCase "SpVector's plus is vaild" $ testPlus
            , testCase "SpVector's dot is vaild" $ testDot
            -- , testCaseSteps "Hastext run on text8_1m" (normalUseCase 10 ["word"] [] text8_1mRunParams)
            , testCaseSteps "Hastext run on text8" (normalUseCase 10 ["king", "woman"] ["man"]
                                                    ((\arg -> arg{_minCount=100}) <$> text8RunParams))
            ]
