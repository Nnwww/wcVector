{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module WordEmbedding.HasText.Internal.Strict.Model
  ( computeHidden
  ) where

import           Control.Concurrent
import qualified Data.Text                            as T
import qualified Data.List                            as L
import qualified Data.IntMap                          as IntMap
import qualified Data.HashMap.Strict                  as HS
import           WordEmbedding.HasText.Internal.Type
                 ( Weights(..)
                 , Dict(..)
                 , Entry(..)
                 , WordVecRef
                 )

computeHidden :: WordVecRef -> Dict -> [T.Text] -> T.Text -> IO (Weights)
computeHidden wsRef Dict{_entries = ents} input target = do
  let inputIDs = map (_eID) . map (ents HS.!) $ input
  ws <- readMVar wsRef
  let targetSpVec = _wI $! ws HS.! target
  pure . Weights $! L.foldl' (\ !spv eid -> IntMap.alter plusOne eid spv) targetSpVec inputIDs
  where
    plusOne (Just v) = Just $! v + 1
    plusOne Nothing  = Just $! 1
