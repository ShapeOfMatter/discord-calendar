module Server where

import Calendar (asICalendar, makeUID)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Discord.Types (GuildId)
import MyDiscord (fetchEvents)
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Text.ICalendar.Printer (printICalendar)

server :: T.Text -> GuildId -> Application
server tok guildid request respond = do
    allEvents <- fetchEvents tok guildid
    let path = pathInfo request
    let showEvents = case path of
                       [] -> allEvents
                       _  -> filter ((`elem` path) . TL.toStrict . makeUID) allEvents
    putStrLn $ "Found " <> show (length showEvents) <> " events in response to request: " <> show path
    respond $ responseLBS
        status200
        [("Content-Type", "text/calendar; charset=utf-8")]  -- could make it nicer, but this is fine.
        (printICalendar def $ asICalendar showEvents)

serve ::  Port -> T.Text -> GuildId -> IO ()
serve port tok guildid = do
    putStrLn $ "http://localhost:" <> show port <> "/"
    run port $ server tok guildid


