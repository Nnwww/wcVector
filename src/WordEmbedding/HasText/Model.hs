{-# LANGUAGE Strict #-}
module WordEmbedding.HasText.Model
  ( Params(..)
  , LParams(..)
  , Model
  , WordVec
  , WordVecRef
  , Weights(..)
  , initLParams
  , updateModel
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Binary                                      (Binary)
import           Data.Binary.Orphans                              ()
import qualified Data.IntMap                     as IntMap
import qualified Data.HashMap.Strict                              as HS
import qualified Data.List                                        as L
import qualified Data.Text                                        as T
import qualified Data.Vector                                      as V
import qualified System.Random.MWC                                as RM
import qualified System.Random.MWC.CondensedTable                 as RMC
import           WordEmbedding.HasText.Dict
import           WordEmbedding.HasText.Internal.Strict.Model
import           WordEmbedding.HasText.Internal.Type              (HasTextArgs (..),
                                                                   LParams (..),
                                                                   Model,
                                                                   Params (..),
                                                                   Weights (..),
                                                                   WordVec,
                                                                   WordVecRef)

instance Binary Weights

initLParams :: RM.GenIO -> LParams
initLParams = LParams 0 IntMap.empty

-- |
-- The function that update a model. This function is a entry point of LParams module.
updateModel :: [T.Text] -> T.Text -> Model
updateModel inputs updTarget = do
  (Params{_wordVecRef = wvRef, _dict = dic}, _) <- ask
  hidden <- liftIO $ computeHidden wvRef dic inputs updTarget
  liftIO $ modifyMVar_ wvRef (pure . HS.insert updTarget hidden)
