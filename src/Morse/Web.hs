{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Morse.Web
 ( waiMorse
 , MorseAPI, morseAPI
 , morseServer
 ) where

import           Data.Text (Text)
import           Data.UUID (UUID)
import           Morse.API
import           Morse.Types
import qualified Network.Wai as WAI
import           Servant

type MorseAPI
 = MorseSay

type MorseSay
  =    "say" :> Capture "uuid" UUID :> Capture "phrase" Text :> Get '[JSON] MorseResponse
  :<|> "say" :>                        Capture "phrase" Text :> Get '[JSON] MorseResponse

morseAPI :: Proxy MorseAPI
morseAPI = Proxy

waiMorse :: (Morse m)
         => (forall a . m a -> Handler a)
         -> (Request -> Text) -- ^ Get a token out of a request
         -> WAI.Application
waiMorse runMorse requestToken = serve morseAPI (hoistServer morseAPI runMorse morseServer)

morseServer :: Morse m => ServerT MorseAPI m
morseServer = sayWith :<|> sayWithout

sayWith :: Morse m => UUID -> Text -> m MorseResponse
sayWith u = say (Just u)

sayWithout :: Morse m => Text -> m MorseResponse
sayWithout = say Nothing

say :: Morse m => Maybe UUID -> Text -> m MorseResponse
say fromState phrase = do
  lookupMorse ("Give me a user token here!"::Text) (fromState, phrase)
