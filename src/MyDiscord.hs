module MyDiscord where

import           Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Discord.Internal.Rest (startRestThread, writeRestCall)
import           Discord.Internal.Types.ScheduledEvents (ScheduledEvent)
import           Discord.Types ( Auth(..)
                               , GuildId)
import qualified Discord.Requests as R
import UnliftIO (Chan, newChan, readChan)
import UnliftIO.Concurrent (forkIO, killThread)


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
