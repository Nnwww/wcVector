{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
module WordEmbedding.HasText.Internal.Strict.Model
  ( computeHidden
  ) where

import           Control.Concurrent
import qualified Data.Text                                        as T
import qualified Data.IntMap                                   as IntMap
import qualified Data.HashMap.Strict                              as HS
import           WordEmbedding.HasText.Internal.Type
                 ( Weights(..)
                 , WordVecRef
                 )

computeHidden :: WordVecRef -> [T.Text] -> IO (Weights)
computeHidden wsRef input = do
  ws <- readMVar wsRef
  pure . Weights . IntMap.unionsWith (+) . map (_wI) . map (ws HS.!) $ input
{-# INLINE computeHidden #-}
