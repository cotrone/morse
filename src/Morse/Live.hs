{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Morse.Live
  ( LiveMorseT
  , UnkFreq
  , runLiveMorseT
  , liveReloader
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Time
import           Data.Bytes.Put
import           Data.Bytes.Serial
import           Data.ByteString
--import           Data.Copointed
import qualified Data.HashPSQ as PSQ
--import           Data.HyperLogLog (HyperLogLog)
--import qualified Data.HyperLogLog as HLL
import           Data.Int
import           Data.Random.Source
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.UUID
import           Morse.API
import           Morse.Types
import Data.Ord

type UnkFreq = (Set ByteString)

-- | A  monad for live Morse usage.
newtype LiveMorseT m a
  = LiveMorseT { unLiveMorseT :: (ReaderT (TVar MorseTree, TVar (PSQ.HashPSQ MorseQuery (Down Int64) UnkFreq)) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Loads the content, and updats it periodicly
liveReloader :: UUID -> Text -> FilePath -> (E.SomeException -> IO ()) -> IO (TVar MorseTree)
liveReloader u d dir handler = E.handle (\(e::E.SomeException) -> handler e >> E.throw e) $ do
  initMT <- load u d dir
  mtTVar <- newTVarIO initMT
  void . forkIO . forever . E.handle (\(e::E.SomeException) -> do { print e; handler e; delay (10::Double); } ) $ do
    newMT <- load u d dir
    atomically $ writeTVar mtTVar newMT
    delay (1::Double)
  pure mtTVar

runLiveMorseT :: MonadIO m => TVar (PSQ.HashPSQ MorseQuery (Down Int64) UnkFreq) -> TVar MorseTree -> LiveMorseT m a -> m a
runLiveMorseT unkTVar mt app = do
  (`runReaderT` (mt, unkTVar)) . unLiveMorseT $ app

instance MonadIO m => Morse (LiveMorseT m) where
  askMorseTree = LiveMorseT ask >>= liftIO . readTVarIO . fst
  logNovel tkn q = LiveMorseT $ do
    p <- ask
    let toPV hll' = let hll = Set.insert (runPutS $ serialize tkn) hll' in ((), (Just (fromIntegral $ Set.size hll, hll)))
    liftIO . atomically $ modifyTVar (snd p) $
      keepSmall . snd . PSQ.alter (\case { Nothing     -> toPV mempty
                                         ; Just (_, s) -> toPV s }) q
    where
      -- | The HLL's random size helps with this, but ...
      keepSmall = id -- FIXME this used to be if PSQ.size psq > 10000 then PSQ.deleteMin psq else psq

{-
    let toPV hll = ((), (Just (copoint $ HLL.size hll, hll)))
    liftIO . atomically $ modifyTVar (fst $ snd p) $
      snd . PSQ.alter (\case { Nothing     -> toPV $ HLL.insert tkn mempty
                             ; Just (_, p) -> toPV $ HLL.insert tkn p }) q
-}

instance MonadIO m => MonadRandom (LiveMorseT m) where
  getRandomWord8  = liftIO $ getRandomWord8
  getRandomWord16 = liftIO $ getRandomWord16
  getRandomWord32 = liftIO $ getRandomWord32
  getRandomWord64 = liftIO $ getRandomWord64
  getRandomDouble = liftIO $ getRandomDouble
  getRandomNByteInteger i = liftIO $ getRandomNByteInteger i
