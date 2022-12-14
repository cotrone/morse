{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Morse.Slack where

import Data.Text
import Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import Data.Hashable
import qualified Data.HashPSQ as PSQ
import Network.HTTP.Types.Status
import Control.Concurrent
import Control.Monad
import System.Environment
import Morse.Types
import Morse.Live
import Data.Int
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
import Data.Ord
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Map.Strict as Map
import System.IO (stdout)
import GHC.IO.Handle (hFlush)

-- Slack formatting

-- | Take n elements with minView
takeMinHashPSQ :: forall k v p. (Hashable k, Ord k, Ord p) => Int -> PSQ.HashPSQ k p v -> ([(k, p, v)], PSQ.HashPSQ k p v)
takeMinHashPSQ takeN i =
  go [] takeN i
  where
    go xs n pq
      | n <= 0 = (xs, pq)
      | otherwise =
        case PSQ.minView pq of
          Nothing -> (xs, pq)
          Just (k, p, v, pq') -> go ((k,p,v):xs) (n - 1) pq'

toSlackMessage :: MorseTree -> [(MorseQuery , Down Int64, UnkFreq)] -> Text
toSlackMessage tree res = TL.toStrict . toLazyText . mconcat $ (\r -> displayResult r <> "\n") <$> res
  where
    displayResult :: (MorseQuery, Down Int64, UnkFreq) -> Builder
    displayResult ((mUUID, queryText), (Down p), _f) =
      Builder.decimal p <> ": " <> fromText queryText <> displayState
      where
        displayState :: Builder
        displayState  = 
          case mUUID of
            Nothing -> mempty
            Just uuid -> " \t " <> (maybe "* NO STATE FOUND *" (fromText . displayTuple) $ Map.lookup uuid (_mtStateDecode tree))
    displayTuple (l, r) = "(" <> l <> ", " <> r <> ")"

-- Slack API

-- | Callback URL
newtype SlackCallbackUrl = SlackCallbackUrl {
  unSlackCabllbackUrl :: String
} deriving (Eq, Ord, Show)

data SlackConnection = SlackConnection {
  connectionManager :: Manager -- ^ An HTTP manager and a token
, connectionCallbackUrl :: SlackCallbackUrl -- ^ The full url for the callback
}

startSlackThread :: SlackConnection -> IO Text -> Int -> IO ()
startSlackThread slackConn getMessage delaySeconds = do
  putStrLn $ "Starting slack posting thread"
  hFlush stdout
  _threadId <- flip forkFinally (\ex -> onException ex >> hFlush stdout) $ do
    initResp <- sendSlackMessage slackConn "Morse moderation bot started"
    unless (successfulResponse initResp) $ do
      putStrLn $ "Unable to send a slack message message: " <> show (responseStatus initResp) 
      hFlush stdout
    forever $ do
      putStrLn "Getting next slack message" 
      hFlush stdout
      msg <- getMessage
      response <- sendSlackMessage slackConn msg
      unless (successfulResponse response) $ do
        putStrLn $ "Unable to send a slack message message: " <> show (responseStatus response) 
        hFlush stdout
      putStrLn "Slack message sent, waiting"
      hFlush stdout
      threadDelay $ (delaySeconds * ((10 :: Int) ^ (6 :: Int)))
  pure ()
  where
    onException (Left ex) = putStrLn $ "Slack thread closed with an exception: "  <> show ex
    onException (Right _) = putStrLn "Slack thread closed gracefully somehow"

-- | Read the slack callback url from an environtment variable SLACK_MESSAGE_CALLBACK_URL
readSlackCallbackUrl :: IO (Either String SlackCallbackUrl)
readSlackCallbackUrl =
  maybe (Left "Unable to lookup SLACK_MESSAGE_CALLBACK_URL environment variable") (Right . SlackCallbackUrl . ("https://hooks.slack.com/services/" <>) . removeQuotes)
    <$> lookupEnv "SLACK_MESSAGE_CALLBACK_URL"
  where
    removeQuotes = Prelude.filter (/= '\'')

initSlack :: SlackCallbackUrl -> IO SlackConnection
initSlack cbUrl = do
  httpManager <- TLS.newTlsManager
  putStrLn $ "Slack connection created with url: " <> unSlackCabllbackUrl cbUrl
  hFlush stdout
  pure $ SlackConnection httpManager cbUrl

successfulResponse :: Response a -> Bool
successfulResponse = (== status200) . responseStatus

sendSlackMessage :: SlackConnection -> Text -> IO (Response Lazy.ByteString)
sendSlackMessage slackConn msg = do
  putStrLn "Attempting to send a slack message"
  hFlush stdout
  let requestUrl = unSlackCabllbackUrl $ connectionCallbackUrl slackConn
  initialRequest <- parseRequest requestUrl
  let reqBody = object ["text" .= msg]
      request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode reqBody }
  httpLbs request $ connectionManager slackConn
