{-# LANGUAGE OverloadedStrings #-}
module TestData where

import           Paths_wcVector
import           Data.Monoid
import           Data.List                  as L
import           Data.Text                  as T
import           WordEmbedding.HasText.Args

noFailInputList :: [Text]
noFailInputList = mconcat . L.map (L.replicate 5 . T.singleton) $ ['a' .. 'e']

noFailPath, text8Path :: IO FilePath
noFailPath = getDataFileName "data/NonFail.txt"
-- text8Path  = getDataFileName "data/text8s/text8_1k"
text8Path  = getDataFileName "data/text8"
noFailParams, noFailOnMultiThreadParams, text8RunParams :: IO HasTextArgs
noFailParams = do
  inputFilePath <- noFailPath
  pure noFailDefault{ _input = inputFilePath, _output = inputFilePath <> ".out"}
noFailOnMultiThreadParams = do
  inputFilePath <- noFailPath
  pure noFailDefault{_input = inputFilePath, _output = inputFilePath <> ".out", _threads = 4}
text8RunParams = do
  inputFilePath <- text8Path
  pure text8RunDefault{_input  = inputFilePath, _output = inputFilePath <> ".out", _threads = 4}

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
