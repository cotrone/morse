{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans
import           Data.Random.Source.StdGen
import           Morse.API
import           Morse.Types
import           Morse.Web
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  mt <- load (read "eccfd622-9197-11eb-8001-8c16454fb02a") "Wut mate?" "content"
  Warp.runEnv 8080 $ waiMorse (\act -> do { g <- liftIO newStdGen; pure $ fst $ runMockMorse mt g act})
