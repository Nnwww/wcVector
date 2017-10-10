{-# LANGUAGE OverloadedStrings #-}
module TestModel where

import           Control.Exception.Safe
import           Control.Monad
import qualified Data.Vector                                   as V
import qualified Data.IntMap                     as IntMap
import           System.IO                                     as SI
import           Test.Tasty.HUnit
import           TestData
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
      step $ "1024byte from text8_1m's head: " ++ show line
      step $ "line length: " ++ show (length line)
      Weights weight <- computeHidden wvRef dic (V.toList line) ("english") -- There is "english" in text8_1m's head
      let numOfElemTo2 = length . filter (\e -> 1.0 < e) . IntMap.elems $ weight
      step $ "weight length: " ++ show (length weight)
      assertBool "The length of input line is equal to weight dim" $ length line == length weight
      assertBool "The number of elements to be 2 is just one" $ numOfElemTo2 == 1
      -- step $ "result of computeHidden" ++ show weight


getLineLoopWithoutDiscards :: Handle -> Dict -> IO (V.Vector Entry)
getLineLoopWithoutDiscards hand dict@Dict{_discards = dis} = do
  isE <- SI.hIsEOF hand
  when isE $ SI.hSeek hand SI.AbsoluteSeek 0
  unsafeGetLine hand dict $ const (pure True)
