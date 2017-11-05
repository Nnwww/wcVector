module TestHasText where

import           WordEmbedding.HasText
import           WordEmbedding.HasText.Internal.Type

import           Data.Monoid
import qualified Data.IntMap                         as IntMap
import qualified System.Directory                    as SD
import qualified Data.Text                           as T

import Test.Tasty.HUnit

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
