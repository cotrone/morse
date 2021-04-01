{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.HashPSQ as PSQ
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Int
import           Morse.Live
import           Morse.Web
import           Morse.Slack
import           Morse.Types
import           Network.Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as WAI
-- import Control.Concurrent (forkIO, threadDelay)
import System.IO (stdout, hFlush)

main :: IO ()
main = do
  mt <- liveReloader (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content" (const $ pure ())
  unk <- newTVarIO PSQ.empty
  eCbURL <- readSlackCallbackUrl
  case eCbURL of
    Left err -> putStrLn ("Unable to read callback url: " <> show err) >> hFlush stdout
    Right cbUrl -> do
      slackConn <- initSlack cbUrl
      startSlackThread  slackConn (getNextSlackMessage 50 mt unk) (5 * 60)
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
