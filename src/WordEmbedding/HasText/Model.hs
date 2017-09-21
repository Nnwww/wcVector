module WordEmbedding.HasText.Model
  ( Params(..)
  , LParams(..)
  , Model
  , WordVec
  , WordVecRef
  , Weights(..)
  , initLParams
  , updateModel
  , genNoiseDistribution
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
  (Params{_wordVecRef = wvRef}, LParams{_hidden = h}) <- ask
  hidden <- liftIO $ computeHidden wvRef inputs
  liftIO $ modifyMVar_ wvRef (pure . HS.insert updTarget hidden)

getNegative :: RMC.CondensedTableV Entry -> RM.GenIO -> T.Text -> IO Entry
getNegative noiseTable rand input = tryLoop
  where
    tryLoop = do
      ent <- RMC.genFromTable noiseTable rand
      if _eWord ent /= input then pure ent else tryLoop

genNoiseDistribution :: Double                    -- ^ nth power of unigram distribution
                     -> TMap Entry                -- ^ vocabulary set for constructing a noise distribution table
                     -> RMC.CondensedTableV Entry -- ^ noise distribution table
genNoiseDistribution power ents =
  RMC.tableFromProbabilities . V.map (second divZ) . V.fromList $ countToPowers
  where
    -- Z is a normalization parameter of the noise distribution in paper.
    divZ a = a / z
    z = L.sum . L.map snd $ countToPowers
    countToPowers = HS.elems . HS.map (id &&& countToPower) $ ents
    countToPower ent = (fromIntegral . _eCount $ ent) ** power

genHierarchical :: TMap Entry -- ^ vocabulary set for building a hierarchical softmax tree
                -> Double           -- ^ learning rate
                -> T.Text           -- ^ a input word
                -> Double           -- ^ loss parameter
genHierarchical ents lr input = undefined
