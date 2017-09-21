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
                 ( SpVector
                 , Weights(..)
                 , WordVecRef
                 )

computeHidden :: WordVecRef -> [T.Text] -> IO (SpVector)
computeHidden wsRef input = do
  ws <- readMVar wsRef
  pure . IntMap.unionsWith (+) . map (_wI) . map (ws HS.!) $ input
{-# INLINE computeHidden #-}
