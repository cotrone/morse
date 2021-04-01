{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Morse.Web
 ( waiMorse
 , MorseAPI, morseAPI
 , morseServer
 , Token (..)
 ) where

import           Data.Text (Text)
import           Data.UUID (UUID)
import           Morse.API
import           Morse.Types
import qualified Network.Wai as WAI
import           Servant
import           Servant.Server.Experimental.Auth

type MorseAPI
 = MorseSay

type MorseSay
  = 
    AuthProtect Token :> ("say" :> Capture "uuid" UUID :> Capture "phrase" Text :> Get '[JSON] MorseResponse
        :<|> "say" :>                   Capture "phrase" Text :> Get '[JSON] MorseResponse)

morseAPI :: Proxy MorseAPI
morseAPI = Proxy

newtype Token = Token { unToken :: Text }

type instance AuthServerData (AuthProtect Token) = Token

type TokenAuthHandler = AuthHandler WAI.Request Token

waiMorse :: (Morse m)
         => (forall a . m a -> Handler a)
         -> (WAI.Request -> Token) -- ^ Get a token out of a request
         -> WAI.Application
waiMorse runMorse requestToken =
  serveWithContext morseAPI context $ hoistServerWithContext morseAPI contextProxy runMorse morseServer
  where
    contextProxy :: Proxy '[TokenAuthHandler]
    contextProxy = Proxy
    context :: Context '[TokenAuthHandler]
    context = authHandler :. EmptyContext
    authHandler = AuthHandler $ pure . requestToken

morseServer :: Morse m => ServerT MorseAPI m
morseServer token = sayWith token :<|> sayWithout token

sayWith :: Morse m => Token -> UUID -> Text -> m MorseResponse
sayWith token u = say (Just u) token

sayWithout :: Morse m => Token -> Text -> m MorseResponse
sayWithout token = say Nothing token

say :: Morse m => Maybe UUID -> Token ->  Text -> m MorseResponse
say fromState (Token t) phrase = do
  lookupMorse t (fromState, phrase)
