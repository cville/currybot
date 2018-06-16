{-# LANGUAGE CPP, OverloadedStrings, DeriveGeneric #-}

-- Extensions to the Web.Slack API
module SlackExtensions where

import Web.Slack
import Web.Slack.Message
import Web.Slack.WebAPI
import Control.Monad.Except
import qualified Data.Text as T



-- Send a rich message using the web API
-- 
--  * This is borrowed from the latest development version of Web.Slack
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

-- Like sendRichMessage but discard any result
sendRichMessageS_ :: SlackConfig -> ChannelId -> T.Text -> [Attachment] -> Slack s ()
sendRichMessageS_ config channel message attachments = do
  liftIO $ sendRichMessage_ config channel message attachments
  
  


-- Escape characters that are control characters in rich messages to Slack
--escapeControlCharacters :: String -> String
--escapeControlCharacters str = fold

-- not probably the best implementation
-- see http://hackage.haskell.org/package/MissingH-1.4.0.1/docs/Data-List-Utils.html for another approach
replace :: Eq a => a -> [a] -> [a] -> [a]
replace orig repl [] = []
replace orig repl (x:xs) = (if x == orig then repl else [x]) ++ (replace orig repl xs)

