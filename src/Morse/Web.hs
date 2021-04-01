{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import           Servant.Server.Experimental.Auth

type MorseAPI
 = MorseSay

type MorseSay
  = AuthProtect Token :> 
      ( "..." :> Capture "uuid" MorseUUID :> Capture "phrase" Text :> Get '[MorseText] MorseResponse
       :<|> "..." :>                             Capture "phrase" Text :> Get '[MorseText] MorseResponse )

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

sayWith :: Morse m => Token -> MorseUUID -> Text -> m MorseResponse
sayWith token u = say token (Just u)

sayWithout :: Morse m => Token -> Text -> m MorseResponse
sayWithout token = say token Nothing

say :: Morse m => Token -> Maybe MorseUUID ->  Text -> m MorseResponse
say (Token t) fromState phraseMorse = do
  let phrase = decodeMorse phraseMorse
  lookupMorse t (unMorseUUID <$> fromState, phrase)

data MorseText

instance Accept MorseText where
  contentType _ = "text" //"x-morse" /: ("charset", "utf-8")

instance MimeRender MorseText MorseResponse where
  mimeRender _ (MorseResponse msg ss) = TLE.encodeUtf8 . TL.fromStrict . encodeMorse $ T.concat [UUID.toText ss, " ", msg]

newtype MorseUUID = MorseUUID { unMorseUUID :: UUID }

instance ToHttpApiData MorseUUID where
  toUrlPiece = encodeMorse . UUID.toText . unMorseUUID
  toQueryParam = encodeMorse . UUID.toText . unMorseUUID
  

instance FromHttpApiData MorseUUID where
  parseUrlPiece = maybe (Left "invalid UUID") Right . fmap MorseUUID . UUID.fromText . decodeMorse
  parseHeader   = maybe (Left "invalid UUID") Right . fmap MorseUUID . UUID.fromText . decodeMorse . TE.decodeUtf8
