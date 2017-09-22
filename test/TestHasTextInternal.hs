module TestHasTextInternal where

import Control.Concurrent
import Debug.Trace
import WordEmbedding.HasText.Dict
import WordEmbedding.HasText.Internal.Strict.HasText
import Test.Tasty.HUnit
import TestData
import qualified Data.HashMap.Strict                 as HS
import qualified Data.IntMap                         as IntMap
import           WordEmbedding.HasText.Internal.Type

testInitWVRef :: Assertion
testInitWVRef = assert $ do
  wv <- readMVar =<< initWVRef =<< initFromFile =<< noFailParams
  pure . IntMap.null . IntMap.filter (/= 1.0) . IntMap.unionsWith (+) . map _wI $ HS.elems wv
