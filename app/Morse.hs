{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.STM
import qualified Data.HashPSQ as PSQ
import qualified Data.Text as Text
import           Morse.Live
import           Morse.Web
import           Morse.Slack
import           Network.Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as WAI
import Morse.Types
import Data.Text (Text)
import Control.Monad
import Data.Ord
import Data.Int

main :: IO ()
main = do
  mt <- liveReloader (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content"
  unk <- newTVarIO PSQ.empty
  startSlackThread (getNextSlackMessage 50 mt unk) (5 * 60)
  Warp.runEnv 8080 $ waiMorse (runLiveMorseT unk mt) requestToken

getNextSlackMessage :: Int -> TVar MorseTree -> TVar (PSQ.HashPSQ MorseQuery (Down Int64) UnkFreq) -> IO Text
getNextSlackMessage i mt unk = do
  (tree, msgs) <- atomically $ do
    pq <- readTVar unk
    _ <- when (PSQ.null pq) retry
    let (msgs, pq') = takeMinHashPSQ i pq
    writeTVar unk pq'
    tree <- readTVar  mt
    pure (tree, msgs)
  pure $ toSlackMessage tree msgs


requestToken :: WAI.Request -> Token
requestToken =
  Token . Text.pack . showRemoteHost . WAI.remoteHost
  where
    showRemoteHost = (\case { SockAddrInet _ ha -> show ha; SockAddrInet6 _ _ ha _ -> show ha; SockAddrUnix s -> s })
