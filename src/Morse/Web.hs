{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Morse.Web
 ( waiMorse
 ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID (UUID)
import           Morse.API
import           Morse.Types
import qualified Network.Wai as WAI
import           Servant

type MorseAPI
 = MorseSay

type MorseSay
  =    "say" :> Capture "uuid" UUID :> CaptureAll "segments" Text :> Get '[JSON] MorseResponse
  :<|> "say" :>                        CaptureAll "segments" Text :> Get '[JSON] MorseResponse

morseAPI :: Proxy MorseAPI
morseAPI = Proxy

waiMorse :: (Morse m)
         => (forall a . m a -> Handler a)
         -> WAI.Application
waiMorse runMorse = serve morseAPI (hoistServer morseAPI runMorse morseServer)

morseServer :: Morse m => ServerT MorseAPI m
morseServer = sayWith :<|> sayWithout

sayWith :: Morse m => UUID -> [Text] -> m MorseResponse
sayWith u = say (Just u)

sayWithout :: Morse m => [Text] -> m MorseResponse
sayWithout = say Nothing

say :: Morse m => Maybe UUID -> [Text] -> m MorseResponse
say fromState parts = do
  let phrase =  T.intercalate " " parts
  lookupMorse (fromState, phrase)
