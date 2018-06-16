{-# LANGUAGE CPP, OverloadedStrings, DeriveGeneric, ViewPatterns #-}

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
import Control.Monad.IO.Class

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
--    Event: normal message of format "echo <rem>"
echoBot config (Message cid _ (T.stripPrefix "echo " -> Just rem) _ Nothing _) = sendMessage cid $ T.append "Echo " rem
--    Event: normal message of format "hoogle <rem>"
echoBot config (Message cid _ (T.stripPrefix "hoogle " -> Just rem) _ Nothing _) = do queryAndSend rem
    where
        -- request data from Hoogle and send result out
        queryAndSend :: Text -> Slack s ()
        queryAndSend msgText = do
            (HoogleResult items) <- getResults msgText
            mapM_ sendItem items
    
        -- get result
        getResults :: Text -> Slack s (HoogleResult)
        getResults queryText = liftIO $ queryAndResolve queryText
    
        -- Query Hoogle and return some kind of HoogleResult even on error.
        -- When that happens, we wrap the error message.
        --
        -- It would be better to propagate an Either (...) for handling later.
        --
        queryAndResolve :: Text -> IO HoogleResult
        queryAndResolve xs = do
          res <- queryHoogle xs
          let resolved = either (const errHoogleResult) id res
              in return resolved
        
        errHoogleResult = HoogleResult [ HoogleItem "#" "Error!" Nothing ]

        -- send result item
        sendItem :: HoogleItem -> Slack s ()
        sendItem x =  sendRichMessageS_ config cid (fmtHoogleItem x) []
                    
     
--    All other events
echoBot config _ = return ()


   
-- Format a HoogleItem as rich text
--
-- You should pass this on to sendRichMessage, as sendMessage doesn't support the extra formatting.
--
fmtHoogleItem :: HoogleItem -> Text
fmtHoogleItem (HoogleItem location self docs) = T.pack $ concat pieces
  where 
    pieces = [ "*Result*", "\n<", location', "|", self', ">\n _", docs', "_" ]
    location' = escapeMessageText location
    self' = escapeMessageText self
    docs' = maybe "" escapeMessageText docs
  
