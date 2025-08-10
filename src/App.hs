module App where

import           Control.Monad (forever, when, void)
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
import           Discord.Internal.Rest (startRestThread, writeRestCall)
import           Discord.Internal.Types.ScheduledEvents (ScheduledEvent)
import           Discord.Types ( Activity(..)
                               , ActivityType(..)
                               , Auth(..)
                               , Event (..)
                               , GatewaySendable(..)
                               , GuildId
                               , messageChannelId
                               , messageContent
                               , messageId
                               , MessageReference(..)
                               , mkActivity
                               , UpdateStatusType(..)
                               , UpdateStatusOpts(..))
import qualified Discord.Requests as R
import MyDiscord
import UnliftIO (Chan, liftIO, newChan, readChan)
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)


fetchEvents :: T.Text -> GuildId -> IO [ScheduledEvent]
fetchEvents tok testserverid = do
  -- SETUP LOG
  printQueue <- newChan :: IO (Chan T.Text)
  printThreadId <- forkIO $ forever $ readChan printQueue >>= TIO.putStrLn

  -- START REST LOOP THREAD
  (restChan, restThreadId) <- startRestThread (Auth tok) printQueue

  -- a rest call to get the channels in which we will post a message
  ecs <- writeRestCall restChan (R.ListScheduledEvents testserverid)
  Right cs <- either (\l -> print l >> (pure $ Left l)) (pure . Right) ecs

  -- CLEANUP
  killThread printThreadId
  killThread restThreadId

  pure cs

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: T.Text -> GuildId -> IO ()
pingpongExample tok testserverid = do
  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler testserverid
                          , discordOnEnd = liftIO $ threadDelay (round @Double (0.4 * 10^(6::Int))) >>  putStrLn "Ended"
                          --, discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          --, discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPresences =True}
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

