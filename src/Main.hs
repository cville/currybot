{-# LANGUAGE CPP, OverloadedStrings, DeriveGeneric #-}

module Main where

-- Slack support
import Web.Slack
import Web.Slack.Message
import Web.Slack.WebAPI
-- grab environment string
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
-- alternate string type
import qualified Data.Text as T
import Data.Text (Text)

-- Ours: methods to query Hoogle for documentation about symbols
import QueryHoogle
-- Ours: additional methods to interact with Slack
import SlackExtensions


-- Get the configuration from the token
getConfig :: String -> SlackConfig
getConfig apiToken = SlackConfig { _slackApiToken = apiToken } 

-- Start things
main :: IO ()
main = do
  -- fetch the token from the environment 
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set") <$> lookupEnv "SLACK_API_TOKEN"
  -- get the configuration
  let config = getConfig apiToken in
    -- execute the event runner
    runBot config (echoBot config) ()

-- Listen to and respond to Slack events
echoBot :: SlackConfig -> SlackBot ()
--    Event: topic changed
echoBot config (Message cid _ msg _ (Just (SChannelTopic _)) _) = sendMessage cid msg
--    Event: normal message
echoBot config (Message cid _ msg _ Nothing _) = case msg of 
    -- respond to a "curry test" request
    "curry test" -> sendRichMessageS_ config cid (fmtHoogleItem hi) []
    otherwise -> sendMessage cid $ T.append "Echo " msg

    where hi = HoogleItem "http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.htmlv:id" "id :: a-&gt;a" (Just "Identity function")
--    All other events
echoBot config _ = return ()


   
-- Format a HoogleItem as rich text
--
-- You should pass this on to sendRichMessage, as sendMessage doesn't support the extra formatting.
--
fmtHoogleItem :: HoogleItem -> Text
fmtHoogleItem (HoogleItem location self docs) = T.pack $ concat [ "*Result*", "\n<", location, "|", self, ">\n _",(maybe "" id docs), "_" ]
