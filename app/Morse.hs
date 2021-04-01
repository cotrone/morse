{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM
import qualified Data.HashPSQ as PSQ
import           Morse.Live
import           Morse.Web
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  mt <- liveReloader (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content"
  unk <- newTVarIO PSQ.empty
  Warp.runEnv 8080 $ waiMorse (runLiveMorseT unk mt)
