{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.STM
import qualified Data.HashPSQ as PSQ
import qualified Data.Text as Text
import           Morse.Live
import           Morse.Web
import           Network.Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as WAI

main :: IO ()
main = do
  mt <- liveReloader (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content"
  unk <- newTVarIO PSQ.empty
  Warp.runEnv 8080 $ waiMorse (runLiveMorseT unk mt) requestToken

requestToken :: WAI.Request -> Token
requestToken =
  Token . Text.pack . showRemoteHost . WAI.remoteHost
  where
    showRemoteHost = (\case { SockAddrInet _ ha -> show ha; SockAddrInet6 _ _ ha _ -> show ha; SockAddrUnix s -> s })
