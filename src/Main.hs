{-# LANGUAGE CPP, OverloadedStrings, DeriveGeneric #-}

module Main where

import Web.Slack
import Web.Slack.Message
import Web.Slack.WebAPI
import Control.Monad.Except
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import qualified Data.Text as T
import Data.Text (Text)
import Control.Concurrent

import RequestDoc


myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }

echoBot :: SlackConfig -> SlackBot ()
echoBot config (Message cid _ msg _ (Just (SChannelTopic _)) _) = sendMessage cid msg
echoBot config (Message cid _ msg _ Nothing _) = case msg of 
    "test" -> sendRichMessageS_ config cid (fmtHoogleItem hi) []
    otherwise -> sendMessage cid $ T.append "Echo " msg

    where hi = HoogleItem "http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.htmlv:id" "id :: a-&gt;a" (Just "Identity function")
echoBot config _ = return ()


main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set")
               <$> lookupEnv "SLACK_API_TOKEN"
  let config = myConfig apiToken in
    runBot config (echoBot config) ()


-- Send a rich message using the web API
-- 
--  * newer versions of Web.Slack support this method already
--  * you can call this without calling runBot!
--
sendRichMessage :: SlackConfig -> ChannelId -> T.Text -> [Attachment] -> IO (Either T.Text ())
sendRichMessage config channel message attachments =
  runExceptT $ chat_postMessage config channel message attachments

-- Like sendRichMessage but discard any result
sendRichMessage_ :: SlackConfig -> ChannelId -> T.Text -> [Attachment] -> IO ()
sendRichMessage_ config channel message attachments = do
  res <- sendRichMessage config channel message attachments
  return ()


-- Like sendRichMessage but discard any result and use Slack return type.
sendRichMessageS_ :: SlackConfig -> ChannelId -> T.Text -> [Attachment] -> Slack s ()
sendRichMessageS_ config channel message attachments = do
  liftIO $ sendRichMessage_ config channel message attachments
  
  

-- Format a HoogleItem as rich text
--
-- You should pass this on to sendRichMessage.
fmtHoogleItem :: HoogleItem -> Text
fmtHoogleItem (HoogleItem location self docs) = T.pack $ concat [ "*Result*", "\n<", location, "|", self, ">\n _",(maybe "" id docs), "_" ]