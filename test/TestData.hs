{-# LANGUAGE OverloadedStrings #-}
module TestData where

import           Control.Concurrent
import           Data.List                                     as L
import           Data.Monoid
import           Data.Text                                     as T
import           Paths_wcVector
import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText.Dict
import           WordEmbedding.HasText.Internal.Strict.HasText
import           WordEmbedding.HasText.Internal.Type

noFailInputList :: [Text]
noFailInputList = mconcat . L.map (L.replicate 5 . T.singleton) $ ['a' .. 'e']

noFailPath, text8Path, text8_1mPath :: IO FilePath
noFailPath = getDataFileName "data/NonFail.txt"
text8Path  = getDataFileName "data/text8"
text8_1mPath = getDataFileName "data/text8s/text8_1m"

noFailParams, noFailOnMultiThreadParams, text8RunParams, text8_1mRunParams :: IO HasTextArgs
noFailParams = do
  inputFilePath <- noFailPath
  pure noFailDefault{ _input = inputFilePath, _output = inputFilePath <> ".out"}
noFailOnMultiThreadParams = do
  inputFilePath <- noFailPath
  pure noFailDefault{_input = inputFilePath, _output = inputFilePath <> ".out", _threads = 4}
text8RunParams = do
  inputFilePath <- text8Path
  pure text8RunDefault{_input  = inputFilePath, _output = inputFilePath <> ".out"}
text8_1mRunParams = do
  inputFilePath <- text8_1mPath
  pure text8RunDefault{_input  = inputFilePath, _output = inputFilePath <> ".out", _minCount = 0}

noFailDefault,text8RunDefault :: HasTextArgs
noFailDefault =  HasTextArgs
  { _input          = ""
  , _output         = ""
  , _initLR         = 0.05
  , _lrUpdateTokens = 100
  , _dim            = 10000
  , _windows        = 1
  , _epoch          = 5
  , _minCount       = 0
  , _negatives      = 5
  , _method         = Cbow
  , _lossFn         = Negative
  , _tSub           = 0.0001
  , _threads        = 1
  , _verbose        = 0
  }
text8RunDefault = noFailDefault
  { _input    = ""
  , _output   = ""
  , _dim      = 10000
  , _windows  = 5
  , _minCount = 10
  , _threads  = 4
  , _verbose  = 1
  }

noFailWordVec :: IO WordVec
noFailWordVec = readMVar =<< initWVRef =<< initFromFile =<< noFailParams

getData :: HasTextArgs -> IO (Dict, WordVecRef)
getData hargs = do
   dic <- initFromFile hargs
   wvRef <- initWVRef dic
   pure (dic, wvRef)
