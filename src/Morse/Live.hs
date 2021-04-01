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
  , runLiveMorseT
  , liveReloader
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Time
import           Data.Random.Source
import qualified Data.Random.Source.MWC as MWC
import           Data.Text (Text)
import           Data.UUID
import           Morse.API
import           Morse.Types

-- | A  monad for live Morse usage.
newtype LiveMorseT m a
  = LiveMorseT { unLiveMorseT :: (ReaderT (TVar MorseTree, MWC.Gen MWC.RealWorld) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Loads the content, and updats it periodicly
liveReloader :: UUID -> Text -> FilePath -> IO (TVar MorseTree)
liveReloader u d dir = do
  initMT <- load u d dir
  mtTVar <- newTVarIO initMT
  void . forkIO . forever . handle (\(e::SomeException) -> do { print e; delay (10::Double); } ) $ do
    newMT <- load u d dir
    atomically $ writeTVar mtTVar newMT
    delay (1::Double)
  pure mtTVar

runLiveMorseT :: MonadIO m => TVar MorseTree -> LiveMorseT m a -> m a
runLiveMorseT mt app = do
  g <- liftIO MWC.create
  (`runReaderT` (mt, g)) . unLiveMorseT $ app

instance MonadIO m => Morse (LiveMorseT m) where
  logNovel _ _ = pure ()
  askMorseTree = LiveMorseT ask >>= liftIO . readTVarIO . fst

askGen :: Monad m => LiveMorseT m (MWC.Gen MWC.RealWorld)
askGen = snd <$> LiveMorseT ask

instance MonadIO m => MonadRandom (LiveMorseT m) where
  getRandomWord8  = askGen >>= liftIO . getRandomWord8From
  getRandomWord16 = askGen >>= liftIO . getRandomWord16From
  getRandomWord32 = askGen >>= liftIO . getRandomWord32From
  getRandomWord64 = askGen >>= liftIO . getRandomWord64From
  getRandomDouble = askGen >>= liftIO . getRandomDoubleFrom
  getRandomNByteInteger i = askGen >>= liftIO . (`getRandomNByteIntegerFrom` i)
