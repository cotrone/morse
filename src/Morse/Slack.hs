{-# LANGUAGE OverloadedStrings #-}
module Morse.Slack where

import Data.Text
import Network.HTTP.Client
import qualified Network.HTTP.Client.TLS as TLS
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import Network.HTTP.Types.Status
import Control.Concurrent
import Control.Monad
import System.Environment

-- | Callback URL
newtype SlackCallbackUrl = SlackCallbackUrl {
  unSlackCabllbackUrl :: String
} deriving (Eq, Ord, Show)

data SlackConnection = SlackConnection {
  connectionManager :: Manager -- ^ An HTTP manager and a token
, connectionCallbackUrl :: SlackCallbackUrl -- ^ The full url for the callback
}

startSlackThread :: IO Text -> Int -> IO ()
startSlackThread getMessage delaySeconds = do
  urlResult <- readSlackCallbackUrl
  case urlResult of
    Left err -> putStrLn $ "Slack posting not running: " <> err
    Right url -> do
      _threadId <- flip forkFinally onException $ do
        slackConn <- initSlack url
        forever $ do
          response <- sendSlackMessage slackConn =<< getMessage
          unless (successfulResponse response) $ putStrLn $ "Unable to send a slack message message: " <> show (responseStatus response) 
          threadDelay $ (delaySeconds * (10 :: Int) ^ (6 :: Int))
      pure ()
  where
    onException (Left ex) = putStrLn $ "Slack thread closed with an exception: "  <> show ex
    onException (Right _) = putStrLn "Slack thread closed gracefully somehow"

-- | Read the slack callback url from an environtment variable SLACK_MESSAGE_CALLBACK_URL
readSlackCallbackUrl :: IO (Either String SlackCallbackUrl)
readSlackCallbackUrl =
  maybe (Left "Unable to lookup SLACK_MESSAGE_CALLBACK_URL environment variable)") (Right . SlackCallbackUrl)
    <$> lookupEnv "SLACK_MESSAGE_CALLBACK_URL"

initSlack :: SlackCallbackUrl -> IO SlackConnection
initSlack cbUrl = do
  httpManager <- TLS.newTlsManager
  pure $ SlackConnection httpManager cbUrl

successfulResponse :: Response a -> Bool
successfulResponse = (== status200) . responseStatus

sendSlackMessage :: SlackConnection -> Text -> IO (Response Lazy.ByteString)
sendSlackMessage slackConn msg = do
  let requestUrl = unSlackCabllbackUrl $ connectionCallbackUrl slackConn
  initialRequest <- parseRequest requestUrl
  let reqBody = object ["text" .= msg]
      request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ encode reqBody }
  httpLbs request $ connectionManager slackConn
