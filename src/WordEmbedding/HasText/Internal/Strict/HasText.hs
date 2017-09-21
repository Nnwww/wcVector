{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict          #-}

module WordEmbedding.HasText.Internal.Strict.HasText
  ( unsafeWindowRange
  , sigmoid
  ) where

import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import qualified System.Random.MWC                   as RM
import           WordEmbedding.HasText.Internal.Type

-- The function that return a range of the dynamic window.
unsafeWindowRange :: HasTextArgs -> LParams -> V.Vector T.Text
                  -> Int -- ^ The central index of a window. Note that no boundary checks.
                  -> IO (V.Vector T.Text)
unsafeWindowRange args lp line targetIdx =
  unsafeWindowRangePrim negs (_rand lp) line targetIdx
  where
    negs = fromIntegral . _negatives $ args

-- The function that return a range of the dynamic window.
unsafeWindowRangePrim :: Int -> RM.GenIO -> V.Vector T.Text
                      -> Int -- ^ The central index of a window. Note that no boundary checks.
                      -> IO (V.Vector T.Text)
unsafeWindowRangePrim negatives rand line targetIdx = do
  winRange <- RM.uniformR (0, negatives) rand
  let winFrom = if targetIdx - winRange > 0 then targetIdx - winRange else 0
      winTo   = if V.length line > targetIdx + winRange then targetIdx + winRange else V.length line - 1
      inWindowAndNotTarget i _ = winFrom < i && i < winTo && i /= targetIdx
  return $ V.ifilter (\i e -> not $ inWindowAndNotTarget i e) line


sigmoid :: Double -> Double
sigmoid lx = 1.0 / (1.0 + exp (negate lx))
