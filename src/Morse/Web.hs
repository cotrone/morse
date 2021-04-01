{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Morse.Web
 ( waiMorse
 , MorseAPI, morseAPI
 , morseServer
 ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Morse.API
import           Morse.Encoding
import           Morse.Types
import Network.HTTP.Media ((//), (/:))
import qualified Network.Wai as WAI
import           Servant

type MorseAPI
 = MorseSay

type MorseSay
  =    "say" :> Capture "uuid" UUID :> Capture "phrase" Text :> Get '[MorseText] MorseResponse
  :<|> "say" :>                        Capture "phrase" Text :> Get '[MorseText] MorseResponse

morseAPI :: Proxy MorseAPI
morseAPI = Proxy

waiMorse :: (Morse m)
         => (forall a . m a -> Handler a)
         -> WAI.Application
waiMorse runMorse = serve morseAPI (hoistServer morseAPI runMorse morseServer)

morseServer :: Morse m => ServerT MorseAPI m
morseServer = sayWith :<|> sayWithout

sayWith :: Morse m => UUID -> Text -> m MorseResponse
sayWith u = say (Just u)

sayWithout :: Morse m => Text -> m MorseResponse
sayWithout = say Nothing

say :: Morse m => Maybe UUID -> Text -> m MorseResponse
say fromState phrase = do
  lookupMorse ("Give me a user token here!"::Text) (fromState, phrase)

data MorseText

instance Accept MorseText where
  contentType _ = "text" //"x-morse" /: ("charset", "utf-8")

instance MimeRender MorseText MorseResponse where
  mimeRender _ (MorseResponse msg ss) = TLE.encodeUtf8 . TL.fromStrict . encodeMorse $ T.concat [UUID.toText ss, " ", msg]

