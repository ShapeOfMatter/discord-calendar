module MyDiscord where

import Calendar (dcIdString, makeUID)
import           Control.Monad (forever, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Discord.Internal.Rest (startRestThread, writeRestCall, RestCallInternalException)
import           Discord.Internal.Types.ScheduledEvents (ScheduledEvent)
import           Discord.Types ( Auth(..)
                               , GuildId)
import qualified Discord.Requests as R
import UnliftIO (Chan, newChan, readChan)
import UnliftIO.Concurrent (forkIO, killThread)

couldHaveID :: ScheduledEvent -> T.Text -> Bool
couldHaveID event idText = idText `elem` [T.pack $ dcIdString event, T.pack $ makeUID event] 

fetchEvents :: T.Text -> GuildId -> IO (Either RestCallInternalException [ScheduledEvent])
fetchEvents tok testserverid = do
  -- SETUP LOG
  printQueue <- newChan :: IO (Chan T.Text)
  printThreadId <- forkIO $ forever $ readChan printQueue >>= TIO.putStrLn

  -- START REST LOOP THREAD
  (restChan, restThreadId) <- startRestThread (Auth tok) printQueue

  -- a rest call to get the channels in which we will post a message
  ecs <- writeRestCall restChan (R.ListScheduledEvents testserverid)
  either print (void . pure) ecs

  -- CLEANUP
  killThread printThreadId
  killThread restThreadId

  pure ecs
