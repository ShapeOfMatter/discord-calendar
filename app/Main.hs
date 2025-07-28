module Main where

import           Control.Monad (join, when, void)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Discord ( DiscordHandler
                         , getGatewayLatency
                         , measureLatency
                         , restCall
                         , runDiscord
                         , RunDiscordOpts(..)
                         , sendCommand)
import           Discord.Types ( Activity(..)
                               , ActivityType(..)
                               , Channel(..)
                               , ChannelId
                               , Event (..)
                               , GatewayIntent(..)
                               , GatewaySendable(..)
                               , GuildId
                               , Message
                               , messageAuthor
                               , messageChannelId
                               , messageContent
                               , messageId
                               , MessageReference(..)
                               , mkActivity
                               , UpdateStatusType(..)
                               , UpdateStatusOpts(..)
                               , userIsBot)
import qualified Discord.Requests as R
import Options.Applicative ( (<**>)
                           , (<|>)
                           , auto
                           , help
                           , helper
                           , info
                           , metavar
                           , option
                           , Parser
                           , progDesc
                           , short
                           , strOption
                           , execParser
                           )
import UnliftIO (liftIO, throwString)
import UnliftIO.Concurrent (threadDelay)




data MainArgs = MainArgs{ botToken :: T.Text
                        , guildID :: GuildId
                        } deriving (Show, Read)

argParser :: Parser (IO MainArgs)
argParser = do
              token <- strOption (short 't' <> metavar "TOKEN" <> help "A token for a bot? DISCOURAGED!")
              guild <- option auto (short 'g' <> metavar "GUILDID" <> help "The guild's ID.")
              pure $ pure $ MainArgs token guild

fileParser :: Parser (IO MainArgs)
fileParser = do
              file <- strOption (short 'f' <> metavar "FILE" <> help "A file from which to read a MainArgs object.")
              pure $ read @MainArgs <$> readFile file


-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  MainArgs{botToken=tok, guildID=testserverid} <- join . execParser $
      info (argParser <|> fileParser <**> helper) (progDesc $ unlines ["A ping-pong bot."])

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler testserverid
                          , discordOnEnd = liftIO $ threadDelay (round @Double (0.4 * 10^(6::Int))) >>  putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          , discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPresences =True}
                          }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: GuildId -> DiscordHandler ()
startHandler testserverid = do
  liftIO $ putStrLn "Started ping-pong bot"

  let activity = (mkActivity "ping-pong" ActivityTypeStreaming) { activityUrl = Just "https://www.youtube.com/watch?v=dQw4w9WgXcQ", activityState = Just "rolling down a hill" }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsActivities = [activity]
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  actionWithChannelId testserverid $ \cid ->
    void $
      restCall $
        R.CreateMessage
          cid
          "Hello! I will reply to pings with pongs"


-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        threadDelay (2 * 10 ^ (6 :: Int))

        -- A very simple message.
        Right m' <- restCall (R.CreateMessage (messageChannelId m) "Pong")
        void $ restCall (R.EditMessage (messageChannelId m, messageId m') (def {R.messageDetailedContent=messageContent m' <> "!"}))

        latency <- getGatewayLatency
        mLatency <- measureLatency

        -- A more complex message. Text-to-speech, does not mention everyone nor
        -- the user, and uses Discord native replies.
        -- Use ":info" in ghci to explore the type
        let opts :: R.MessageDetailedOpts
            opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!. Here's the current gateway latency: " <> (T.pack . show) ([latency, mLatency])
                       , R.messageDetailedTTS = True
                       , R.messageDetailedAllowedMentions = Just $
                          def { R.mentionEveryone = False
                              , R.mentionRepliedUser = False
                              }
                       , R.messageDetailedReference = Just $
                          def { referenceMessageId = Just $ messageId m }
                       }
        void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
      _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId testserverid f = do
  response <- restCall $ R.GetGuildChannels testserverid
  chans <- case response of
    Left err -> throwString $ show err
    Right c -> pure c
  ch : _ <- pure $ filter isTextChannel chans
  f . channelId $ ch
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False
