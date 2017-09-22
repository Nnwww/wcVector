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
  pure . Weights $! L.foldr (\eid !spv -> IntMap.alter plus eid spv) targetSpVec inputIDs
  where
    plus (Just v) = Just $! v + 1
    plus Nothing  = Just $ 1
