{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Monad.IO.Class (liftIO)
import Control.Lens.Getter (view)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Lambdabot.Main
import Modules (modulesInfo)
import Prelude hiding (dropWhile, filter)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)
import Web.Slack (Event(Message), SlackBot, SlackConfig(..), runBot)
import Web.Slack.Message (sendMessage)
import Web.Slack.Types

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

-- | Run one or more commands against Lambdabot and capture the response.
lambdabot :: String -> Submitter -> String -> IO String
lambdabot admin submitter command = do
  let userFromComment (UserComment uid) = uid
  let submitterId = unpack $ view getId $ userFromComment submitter
  let request = void $ lambdabotMain modulesInfo
        [ onStartupCmds ==> [command]
        , adminUser ==> admin
        , requestingUser ==> submitterId
        ]
  (response, _) <- capture request
  return response

-------------------------------------------------------------------------------
-- Slack
-------------------------------------------------------------------------------

-- | Construct a @SlackConfig@, taking the Slack API token from an environment
-- variable.
envMkSlackConfig :: String -> IO SlackConfig
envMkSlackConfig key
  =  mkSlackConfig
 <$> fromMaybe (error $ key <> " not set")
 <$> lookupEnv key

-- | Construct a @SlackConfig@ from a Slack API token.
mkSlackConfig :: String -> SlackConfig
mkSlackConfig apiToken = SlackConfig { _slackApiToken = apiToken }

isCommand :: String -> Bool
isCommand cmd = any (\c -> c `isPrefixOf` cmd) ["!", "="]

getCommand :: String -> Maybe String
getCommand cmd = if isCommand cmd then Just cmd else Nothing

-- | Construct a @SlackBot@ from a name. This bot will pass messages addressed
-- to it to 'lambdabot' and relay 'lambdabot''s response.
-- TChannel -> Event.Submitter -> Text -> SlackTimeStamp -> Subtype.Subtype -> Item.Edited
slackBot :: SlackBot a
slackBot (Message chanid submitter message _ _ _) = case command of
  Just cmd -> do
    admin <- liftIO $ requireEnv "ADMIN_ID"
    resp <- liftIO (pack . decodeString <$> (lambdabot admin submitter cmd))
    sendMessage chanid ("```\n" <> resp <> "```")
  Nothing -> return ()
  where
    command = getCommand $ decodeHtml message
slackBot _ = return ()

decodeHtml :: Text -> String
decodeHtml = unpack . toStrict . toLazyText . htmlEncodedText

requireEnv :: String -> IO String
requireEnv key = fromMaybe (error $ key <> " not set") <$> lookupEnv key

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  admin <- requireEnv "ADMIN_ID" -- force env check on startup
  putStrLn $ "Admin is " ++ admin
  slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
  runBot slackConfig slackBot ()
