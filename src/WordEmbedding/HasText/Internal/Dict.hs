{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WordEmbedding.HasText.Internal.Dict where

import qualified Data.Char                           as C
import           Data.Conduit
import qualified Data.Conduit.Combinators            as CC
import qualified Data.HashMap.Strict                 as HS
import qualified Data.Text                           as T
import qualified Data.Vector                         as V
import           System.IO                           as SI
import           WordEmbedding.HasText.Internal.Type (Dict (..), Entry (..))

unsafeGetLine :: Handle -> Dict -> (T.Text -> IO Bool) -> IO (V.Vector Entry)
unsafeGetLine h Dict{..} filterPred =
  runConduit $ CC.sourceHandle h
    .| CC.takeE 1024
    .| CC.decodeUtf8
    .| CC.takeWhileE (/= '\n')
    .| CC.splitOnUnboundedE C.isSpace
    .| CC.filterM filterPred
    .| CC.map (_entries HS.!)
    .| CC.sinkVector
