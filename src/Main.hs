{-# LANGUAGE CPP #-}

module Main where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import qualified Data.Text as T

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }

echoBot :: SlackBot ()
echoBot (Message cid _ msg _ (Just (SChannelTopic _)) _) = sendMessage cid msg
echoBot (Message cid _ msg _ Nothing _) = sendMessage cid (T.append (T.pack "Echo ") msg)
echoBot _ = return ()

main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
               <$> lookupEnv "SLACK_API_TOKEN"
  runBot (myConfig apiToken) echoBot ()

