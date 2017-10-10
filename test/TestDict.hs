{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module TestDict where

import WordEmbedding.HasText.Dict
import TestData

import Debug.Trace
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Test.Tasty.HUnit

instance Eq Entry where
  (==) (Entry wordA countA iDA)
       (Entry wordB countB iDB) = (wordA == wordB) && (countA == countB) && (iDA == iDB)

testaddEntryWithID :: Assertion
testaddEntryWithID = assert (assumption == testAEData)
  where
    testAEData = fadd "b" . fadd "a" . fadd "b" $ (0, HS.empty)
    fadd = flip addEntryWithID
    assumption = (2,) $ HS.fromList [("a", Entry{_eWord = "a", _eCount = 1 , _eID = 1}),
                                     ("b", Entry{_eWord = "b", _eCount = 2 , _eID = 0})]

listWordsFromFile :: FilePath -> IO [T.Text]
listWordsFromFile fp = foldWordsFromFile (\a t -> t : a) [] fp

testReadCorrectlyWordsFromFile :: Assertion
testReadCorrectlyWordsFromFile = assert $ do
  input <- listWordsFromFile =<< noFailPath
  pure $ noFailInputList == L.reverse input

testCollectFromFile :: Assertion
testCollectFromFile = assert $ do
  ents <- foldWordsFromFile addEntryWithID (0, HS.empty) =<< noFailPath
  pure $ ents == assumption
    where
      assumption = (5,) . HS.fromList . L.zipWith makekv [0..] . L.map T.singleton $ ['a' .. 'e']
      makekv i w = (w, Entry w 5 i)

testInitFromFile :: Assertion
testInitFromFile = assert $ do
  dict@(Dict ents diss _) <- initFromFile =<< noFailParams
  -- print dict
  pure $ HS.size ents /= 0 && HS.size diss /= 0
