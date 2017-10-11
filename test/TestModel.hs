{-# LANGUAGE OverloadedStrings #-}
module TestModel where

import           Control.Exception.Safe
import           Control.Monad
import qualified Data.Vector                                   as V
import qualified Data.IntMap                                   as IntMap
import qualified Data.Set                                      as Set
import           System.IO                                     as SI
import           Test.Tasty.HUnit
import           TestData
import           WordEmbedding.HasText.Model
import           WordEmbedding.HasText.Internal.Dict
import           WordEmbedding.HasText.Internal.Type
import           WordEmbedding.HasText.Internal.Strict.Model

testComputeHidden :: (String -> IO ()) -> Assertion
testComputeHidden step = do
  hargs <- text8_1mRunParams
  bracket (SI.openFile (_input hargs) SI.ReadMode) SI.hClose (testCHMain hargs)
  where
    testCHMain arg inputPath = do
      step $ "make Dict and WordVecRef"
      (dic, wvRef) <- getData arg
      step $ "get a line sentence"
      line <- V.map _eWord <$> getLineLoopWithoutDiscards inputPath dic
      let noDupList = Set.toList . Set.fromList . V.toList $ line
      Weights weight <- computeHidden wvRef dic noDupList ("word") -- There is "word" in the head 1024byte of text8
      let numOfElemTo2 = length . filter (\e -> 1.0 < e) . IntMap.elems $ weight
      assertEqual "The length of input line removed duplication is equal to weight dim" (length noDupList) (length weight)
      assertEqual "The number of elements to be 2 is just one" 1 numOfElemTo2

getLineLoopWithoutDiscards :: Handle -> Dict -> IO (V.Vector Entry)
getLineLoopWithoutDiscards hand dict = do
  isE <- SI.hIsEOF hand
  when isE $ SI.hSeek hand SI.AbsoluteSeek 0
  unsafeGetLine hand dict $ const (pure True)
