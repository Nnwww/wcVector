{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TupleSections   #-}

module WordEmbedding.HasText.Internal.Strict.HasText
  ( unsafeWindow
  , unsafeDynamicWindow
  , unsafeWindowPrim
  , sigmoid
  , initWVRef
  ) where

import           Control.Arrow
import           Control.Concurrent
import qualified Data.IntMap                         as IntMap
import qualified Data.HashMap.Strict                 as HS
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import qualified System.Random.MWC                   as RM
import           WordEmbedding.HasText.Internal.Type

-- The function that return a range of the dynamic window.
unsafeDynamicWindow :: HasTextArgs -> LParams -> V.Vector T.Text
                    -> Int -- ^ The central index of a window. Note that no boundary checks.
                    -> IO (V.Vector T.Text)
unsafeDynamicWindow args LParams{_rand = rand} line targetIdx =
  dynamicWindow <$> RM.uniformR (0, win) rand
  where
    dynamicWindow dynWin = unsafeWindowPrim dynWin line targetIdx
    win = fromIntegral . _windows $ args

-- The function that return a range of the dynamic window.
unsafeWindow :: HasTextArgs -> V.Vector T.Text
             -> Int -- ^ The central index of a window. Note that no boundary checks.
             -> V.Vector T.Text
unsafeWindow args line targetIdx = unsafeWindowPrim win line targetIdx
  where
    win = fromIntegral . _windows $ args

-- The function that return a range of the dynamic window.
unsafeWindowPrim :: Int -> V.Vector T.Text
                 -> Int -- ^ The central index of a window. Note that no boundary checks.
                 -> V.Vector T.Text
unsafeWindowPrim winRange line targetIdx = inFrontOfTarget V.++ inRearOfTarget
  where
    inFrontOfTarget = if targetIdx - 1 <= 0
                      then V.fromList []
                      else V.unsafeSlice winFrom (targetIdx - winFrom) line
    inRearOfTarget  = if V.length line - 1 <= targetIdx + 1
                      then V.fromList []
                      else V.unsafeSlice (targetIdx + 1) (winTo - targetIdx) line
    winFrom = if targetIdx - winRange <= 0 then 0 else targetIdx - winRange
    winTo   = if V.length line - 1 <= targetIdx + winRange then V.length line - 1 else targetIdx + winRange

sigmoid :: Double -> Double
sigmoid lx = 1.0 / (1.0 + (exp $! negate lx))

initWVRef :: Dict -> IO WordVecRef
initWVRef Dict{_entries = ents} = newMVar . HS.fromList . map wordAndWeights . HS.keys $ ents
  where
    wordAndWeights :: T.Text -> (T.Text, Weights)
    wordAndWeights = id &&& initW
    initW key = Weights $! IntMap.insert (_eID $! ents HS.! key) 1 IntMap.empty
