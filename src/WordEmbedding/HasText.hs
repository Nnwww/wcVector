{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module WordEmbedding.HasText
  ( HasTextResult(..)
  , train
  , loadModel
  , loadVectorCompat
  , saveModel
  , saveVectorCompat
  , ErrMostSim(..)
  , mostSimilar
  , mostSimilarN
  ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.ST
import qualified Data.Binary                                   as B
import qualified Data.Binary.Get                               as BG
import qualified Data.HashMap.Strict                           as HS
import           Data.Mutable
import           Data.Ord
import           Data.IntMap                                   (IntMap)
import qualified Data.IntMap                                   as IntMap
import           Data.Semigroup
import qualified Data.Text                                     as T
import qualified Data.Text.IO                                  as TI
import qualified Data.Vector                                   as V
import qualified Data.Vector.Algorithms.Intro                  as VA
import qualified Data.Vector.Unboxed                           as VU
import qualified System.IO                                     as SI
import qualified System.ProgressBar                            as P
import qualified System.Random.MWC                             as RM
import           TextShow
import           WordEmbedding.HasText.Args
import           WordEmbedding.HasText.Dict
import           WordEmbedding.HasText.Internal.Strict.HasText
import           WordEmbedding.HasText.Internal.Type           (HasTextResult (..))
import           WordEmbedding.HasText.Internal.SpVector
import           WordEmbedding.HasText.Model

instance B.Binary HasTextResult where
  get = do
    a <- B.get
    d <- B.get
    w <- B.get
    return HasTextResult
      { htArgs      = a
      , htDict      = d
      , htNoiseDist = genNoiseDistribution 0.75 $ _entries d
      , htWordVec   = w
      }
  put HasTextResult{htArgs = a, htDict = d, htWordVec = w} = B.put a >> B.put d >> B.put w

skipgram :: V.Vector T.Text -> Model
skipgram line = forM_ [0..V.length line - 1] $ \idx -> do
  (Params{_args = a}, lp) <- ask
  mapM_ (learn $ V.unsafeIndex line idx) =<< liftIO (unsafeWindowRange a lp line idx)
  where
    learn input target = updateModel [input] target

cbow :: V.Vector T.Text -> Model
cbow line = forM_ [0..V.length line - 1] $ \idx -> do
  (a, lp) <- asks $ first _args
  updateRange <- liftIO $ unsafeWindowRange a lp line idx
  updateModel (V.toList updateRange) (V.unsafeIndex line idx)

-- TODO: compare parallelization using MVar with one using ParIO etc.
trainThread :: Params -> Integer -> IO Params
trainThread params@Params{_args = args, _dict = dict, _tokenCountRef = tcRef, _progLogger = logger} threadNo = do
  genRand <- RM.createSystemRandom
  bracket (SI.openFile (_input args) SI.ReadMode) SI.hClose (trainMain genRand)
  return params
  where
    allTokens = _epoch args * _ntokens dict
    method    = chooseMethod . _method $ args
    chooseMethod Cbow     = cbow
    chooseMethod Skipgram = skipgram

    trainMain :: RM.GenIO -> SI.Handle -> IO ()
    trainMain rand h = do
      size <- SI.hFileSize h
      SI.hSeek h SI.AbsoluteSeek $ size * threadNo `quot` (fromIntegral $ _threads args)
      let trainUntilCountUpTokens !localTC oldLParams@LParams{_lr = oldLR} = do
            tokenCount <- atomicModifyRef' tcRef (\tc -> (tc,fromIntegral tc))
            if allTokens < tokenCount then return ()
              else do
              let (progress :: Double) = fromIntegral tokenCount / fromIntegral allTokens
                  newLParams           = oldLParams{_lr = oldLR * (1.0 - progress)}
              line <- getLineLoop h dict rand
              let learning = method $ V.map _eWord line
              runReaderT learning (params, newLParams)
              newLocalTC <- reportTokenCountBuffered $ localTC + fromIntegral (V.length line)
              trainUntilCountUpTokens newLocalTC newLParams
      trainUntilCountUpTokens 0 $ initLParams rand

    reportTokenCountBuffered :: Word -> IO Word
    reportTokenCountBuffered localTokenCount
      | localTokenCount <= _lrUpdateTokens args = return localTokenCount
      | otherwise = do
         atomicModifyRef' tcRef $ (,()) . (+ localTokenCount)
         logger $ localTokenCount
         pure 0


train :: HasTextArgs -> IO HasTextResult
train args = do
  check
  dict  <- initFromFile args
  wvRef <- initWVRef dict
  tcRef <- newRef 0
  switchLoggingFunction dict $ \logger -> do
    let params = Params
          { _args          = args
          , _dict          = dict
          , _noiseDist     = genNoiseDistribution 0.75 $ _entries dict
          , _wordVecRef    = wvRef
          , _tokenCountRef = tcRef
          , _progLogger    = logger
          }
    resultParams : _ <- mapConcurrently (trainThread params) [0.. fromIntegral $ _threads args - 1]
    immWordVec <- readMVar $ _wordVecRef resultParams
    pure HasTextResult
      { htArgs      = _args      resultParams
      , htDict      = _dict      resultParams
      , htNoiseDist = _noiseDist resultParams
      , htWordVec   = immWordVec
      }
  where
    check = validArgs args >>= (flip unless) (throwString "Error: Invalid Arguments.")
    initWVRef :: Dict -> IO WordVecRef
    initWVRef Dict{_entries = ents} = newMVar . HS.fromList . map wordAndWeights . HS.keys $ ents
      where
        wordAndWeights :: T.Text -> (T.Text, Weights)
        wordAndWeights = id &&& initW
        initW key = Weights $ IntMap.insert (_eID $ ents HS.! key) 1 zeroSpVector
        zeroSpVector = IntMap.fromList . map ((, 0) . _eID) . HS.elems $ ents

    switchLoggingFunction dict
      | _verbose args == 0 = \run -> run (const $ pure ())
      | otherwise = bracketProgressThread (P.startProgress (P.msg "Training") P.percentage 40 allTokens)
      where
        allTokens = fromIntegral $ (_epoch args) * (_ntokens dict)

    bracketProgressThread acquire run = do
      (progRef, progThId) <- acquire
      result <- run $ logTokenCount progRef
      killThread progThId
      pure result

    logTokenCount :: P.ProgressRef -> Word -> IO ()
    logTokenCount progRef localTokenCount = P.incProgress progRef $ fromIntegral localTokenCount

data ErrMostSim = EmptyInput
                | AbsenceOfWords {absPosW :: [T.Text], negPosW :: [T.Text]}
                -- ^ words that do not exist in trained corpora when execute mostSimilar.

-- | Get a most similar word list. Note that the result list is a delayed version of the entire dictionary.
mostSimilar :: HasTextResult
            -> Word     -- ^ from
            -> Word     -- ^ to
            -> [T.Text] -- ^ positive words
            -> [T.Text] -- ^ negative words
            -> Either ErrMostSim [(T.Text, Double)]
mostSimilar HasTextResult{htWordVec = wv} from to positives negatives
  | length positives == 0 && length negatives == 0 = Left EmptyInput
  | length absPoss   /= 0 || length absNegs   /= 0 = Left $ AbsenceOfWords absPoss absNegs
  | otherwise = Right . slice from to $ V.toList sortedCosSims
  where
    sortedCosSims = runST $ do
      cosSimVecs <- V.unsafeThaw . V.map (second $ cosSim mean . _wI) . V.fromList $ HS.toList wv
      VA.sortBy (flip $ comparing snd) cosSimVecs
      V.unsafeFreeze cosSimVecs
    mean         = scale (1 / inputLength) $ foldr1 plus scaledInputs
    scaledInputs = map getPosScale positives <> map getNegScale negatives
    absPoss      = absentWords positives
    absNegs      = absentWords negatives
    absentWords  = filter (not . flip HS.member wv)
    inputLength  = fromIntegral $ (length positives) + (length negatives)
    getPosScale  = getVec
    getNegScale  = scale (-1) . getVec
    getVec       = _wI . (wv HS.!)
    slice f t xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)
      where
        start = fromIntegral f
        stop  = fromIntegral t

-- | Such synonym of mostSimilar as it return from 0 to top N.
mostSimilarN :: HasTextResult
            -> Word     -- ^ top N
            -> [T.Text] -- ^ positive words
            -> [T.Text] -- ^ negative words
            -> Either ErrMostSim [(T.Text, Double)]
mostSimilarN w topn positives negatives = mostSimilar w 0 topn positives negatives

-- | This function save a trained data, but you don't have to use this because this is essentially a wrapper function of Binary package at present.
saveModel :: (MonadIO m, MonadThrow m) => HasTextResult -> m ()
saveModel w@HasTextResult{htArgs = args} = liftIO $ B.encodeFile outFilePath w
  where
    outFilePath = _output args

saveVectorCompat :: (MonadIO m, MonadThrow m) => HasTextResult -> m ()
saveVectorCompat HasTextResult{htArgs = args, htDict = dict, htWordVec = wv} =
  liftIO . SI.withFile (outFilePath <> ".vecc") SI.WriteMode $ \h -> do
    TI.hPutStrLn h $ toText sizeAndDim
    mapM_ (putVec h) $ HS.toList wv
  where
    outFilePath = _output args
    sizeAndDim = (showb . HS.size $ _entries dict) <> showbSpace <> (showb $ _dim args)
    putVec h (k, Weights{_wI = i}) =
      TI.hPutStrLn h . toText $ (fromText k) <> showbSpace <> (unwordsB . map showb $ IntMap.toList i)

-- | This function load a pre-trained data, but you don't have to use this because this is essentially a wrapper function of Binary package at present.
loadModel :: (MonadIO m) => FilePath -> m (Either (BG.ByteOffset, String) HasTextResult)
loadModel fpath = liftIO $ B.decodeFileOrFail fpath

loadVectorCompat :: (MonadIO m, MonadThrow m) => FilePath -> m HasTextResult
loadVectorCompat fpath = undefined
