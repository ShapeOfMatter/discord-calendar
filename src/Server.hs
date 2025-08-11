module Server where

import Calendar (asICalendar, makeUID)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.Default (def)
import Data.List (elemIndices, unsnoc)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Discord.Types (GuildId)
import           Discord.Internal.Types.ScheduledEvents (ScheduledEvent)
import MyDiscord (fetchEvents)
import Network.HTTP.Types (Status, status200, badRequest400, notFound404)
import Network.Wai (Application, pathInfo, responseLBS, Response)
import Network.Wai.Handler.Warp (Port, run)
import Text.ICalendar.Printer (printICalendar)
import Text.Read (readMaybe)

utf8 :: String -> LazyByteString
utf8 = toLazyByteString . stringUtf8

type EventId = T.Text
type Path = [T.Text]

data FileType = ICSFile
               | NullFile
               deriving (Bounded, Enum, Eq, Ord, Read, Show) -- I assume Ord uses the order above, which could matter if I mess up the extensions list.

contentType :: FileType -> ByteString
contentType ICSFile = "text/calendar; charset=utf-8"
contentType NullFile = "text/plain; charset=utf-8"

extensionType :: String -> Either String FileType
extensionType "" = Right NullFile
extensionType "ics" = Right ICSFile
extensionType unknown = Left unknown

data GoodRequest = GuildRequest FileType GuildId
                 | EventRequest FileType GuildId EventId
                 deriving (Read, Show)

data BadRequest  = UnknownPath Path (Maybe String)
                 | UnknownFileType Path String
                 | UnknownEvent GuildId EventId
                 | UnparseableFields Path (Maybe String)
                 deriving (Read, Show)

errorMessages :: BadRequest -> [String]
errorMessages (UnknownPath path message) = ["There is no service at that path.", "Path: " <> show path] <> maybeToList message
errorMessages (UnknownFileType path extension) = ["I do not recognize the requested file type.", "Path: " <> show path, "Extension: " <> extension]
errorMessages (UnknownEvent guildid eventid) = ["Couldn't find the requested event at the specified guild (discord server)."
                                               ,"The event may be over, or you may have the wrong ID, or there may be some other problem."
                                               ,"Guild ID: " <> show guildid
                                               ,"Event ID: " <> show eventid]
errorMessages (UnparseableFields path message) = ["Request fields are not parseable.", "Path: " <> show path] <> maybeToList message

status :: BadRequest -> Status
status (UnknownPath _ _) = notFound404
status (UnknownFileType _ _) = badRequest400
status (UnknownEvent _ _) = notFound404
status (UnparseableFields _ _) = badRequest400

textError :: BadRequest -> Response
textError r = responseLBS (status r) [("Content-Type", contentType NullFile)] (utf8 . unlines $ errorMessages r)

splitExtension :: Path -> (Path, String)  -- Should be able to do this all in T.Text, but i'm lazy.
splitExtension path = case unsnoc path of
  Nothing -> ([], "")  -- empty path
  Just (ps, pText) -> let p = T.unpack pText in case unsnoc (elemIndices '.' p) of
                    Nothing -> (ps <> [pText], "")  -- No extension
                    Just (_, lastIndex) -> case splitAt lastIndex p of
                                             ("", _) -> (ps <> [pText], "") -- starts with dot; don't interpret as extension
                                             (name, '.' : extension) -> (ps <> [T.pack name], extension)  -- found a real extension
                                             (_, _) -> undefined -- can't happen

parseGuildID :: T.Text -> Maybe GuildId
parseGuildID = readMaybe . T.unpack

parseRequest :: [T.Text] -> Either BadRequest GoodRequest
parseRequest rawpath = 
  let (path, eitherFileType) = extensionType <$> splitExtension rawpath
  in case path of
       [] -> Left $ UnknownPath path (Just "Ask Mako for help using this service.")
       rawguildid : eventids | Just guildid <- parseGuildID rawguildid ->
               case (eventids, eitherFileType) of ([], Right ft) -> Right $ GuildRequest ft guildid
                                                  ([eventid], Right ft) -> Right $ EventRequest ft guildid eventid
                                                  (_:_, _) -> Left $ UnknownPath path (Just "Try just one guildID and one EventID.")
                                                  (_, Left ext) -> Left $ UnknownFileType path ext
       _ -> Left $ UnparseableFields path (Just "Couldn't parse the GuildId.")

server :: T.Text -> Application
server tok rawrequest respond = do
    let request = parseRequest $ pathInfo rawrequest
    response <- case request of
      Left badRequest -> pure $ textError badRequest
      Right goodRequest -> handle tok goodRequest
    respond response
 
handle :: T.Text -> GoodRequest -> IO Response
handle tok (GuildRequest ft guildid) = do
  events <- fetchEvents tok guildid
  putStrLn $ "Found " <> show (length events) <> " events for guild: " <> show guildid
  pure $ successResponse events ft
handle tok (EventRequest ft guildid eventid) = do
  allEvents <- fetchEvents tok guildid
  case filter ((== eventid) . TL.toStrict . makeUID) allEvents of
    [] -> pure $ textError $ UnknownEvent guildid eventid
    [event] -> do putStrLn $ "Found event for (guild, event): " <> show (guildid, eventid)
                  pure $ successResponse [event] ft
    events@(_:_) -> do putStrLn $ "FOUND " <> show (length events) <> " EVENTS FOR (GUILD, EVENT): " <> show (guildid, eventid)
                       pure $ successResponse events ft

successResponse :: [ScheduledEvent] -> FileType -> Response
successResponse events ft = responseLBS status200 [("Content-Type", contentType ft)] case ft of
  ICSFile -> (printICalendar def $ asICalendar events)
  NullFile -> utf8 $ show events


serve ::  Port -> T.Text -> IO ()
serve port tok = do
    putStrLn $ "http://localhost:" <> show port <> "/"
    run port $ server tok


