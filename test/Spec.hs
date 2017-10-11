{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Test.Tasty
import Test.Tasty.HUnit

import           TestDict
import           TestModel
import           TestData
import           TestHasTextInternal

import           Control.DeepSeq
import           Data.Monoid
import qualified Data.Text                   as T
import qualified System.Directory            as SD
import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText
import           WordEmbedding.HasText.Internal.Type

normalUseCase :: Word -> [T.Text] -> [T.Text] -> IO HasTextArgs -> ((String -> IO ()) -> Assertion)
normalUseCase topn posWords negWords args step = do
  a@HasTextArgs{_input = i, _output = o} <- args
  step $ "input path: "  <> i
  step $ "output path: " <> o
  step "Running train"
  w <- train a
  -- step "Show Entries:"
  -- step . show . _entries $ htDict w
  -- step "Show WordVec:"
  -- step . show $ htWordVec w
  step "Running mostSim"
  let Right r = mostSimilarN w topn posWords negWords
  step ("Top " <> show topn <> " of mostSimilar: " <> show r)
  step "Running saveModel"
  saveModel w
  let outputPath = _output . htArgs $ w
  existanceOutputModel <- SD.doesFileExist outputPath
  assertEqual "save a model" True existanceOutputModel
  step "Running saveVecCompat"
  saveVectorCompat w
  existanceOutputVectorCompat <- SD.doesFileExist (outputPath <> ".vecc")
  assertEqual "Save vectors as a compatible form." True existanceOutputVectorCompat
  step "Running loadModel"
  Right _ <- loadModel (_output a)
  return ()

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
            -- , testCaseSteps "Hastext run on text8_1m" (normalUseCase 10 ["word"] [] text8_1mRunParams)
            , testCaseSteps "Hastext run on text8" (normalUseCase 10 ["king", "woman"] ["man"] text8RunParams)
            ]
