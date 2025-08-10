module Server where

import App (fetchEvents)
import Calendar (asICalendar, makeUID)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Discord.Types (GuildId)
import Network.HTTP.Types (status200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import Text.ICalendar.Printer (printICalendar)

server :: T.Text -> GuildId -> Application
server tok guildid request respond = do
    allEvents <- fetchEvents tok guildid
    let path = pathInfo request
    let showEvents = case path of
                       [] -> allEvents
                       _  -> filter ((`elem` path) . TL.toStrict . makeUID) allEvents
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/calendar; charset=utf-8")]  -- could make it nicer, but this is fine.
        (printICalendar def $ asICalendar showEvents)

serve ::  T.Text -> GuildId -> IO ()
serve tok guildid = do
    putStrLn $ "http://localhost:8080/"
    run 8080 $ server tok guildid


