module Calendar where

import Data.Default (def)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Word (Word64)
import qualified Discord.Internal.Types as DT
import qualified Discord.Internal.Types.Prelude as DP
import qualified Discord.Internal.Types.ScheduledEvents as DC
import Data.Time.Clock (UTCTime(UTCTime))
import GHC.IsList (fromList)
import Codec.MIME.Type (MIMEType(Image))
import Network.URI (URI(..), URIAuth(..))
import qualified Text.ICalendar as IC

type ICalendar = IC.VCalendar

asICalendar :: [DC.ScheduledEvent] -> ICalendar
asICalendar ses = def{ IC.vcEvents = M.fromList [((makeUID se, Nothing), asICalEvent se) | se <- ses] }

asICalEvent :: DC.ScheduledEvent -> IC.VEvent
asICalEvent se = IC.VEvent {
    IC.veDTStamp = IC.DTStamp (UTCTime (toEnum 0) (toEnum 0)) def,  -- Wish i could omit this.
    IC.veUID = IC.UID (makeUID se) def,
    IC.veClass = asICPrivacyClass . dcPrivacyLevel $ se,
    IC.veDTStart = Just . (`IC.DTStartDateTime` def) . IC.UTCDateTime . dcStartTime $ se,
    IC.veCreated = Nothing,  -- We don't seem to have it and probably don't care.
    IC.veDescription = icTextVal IC.Description <$> dcDescription se,
    IC.veGeo = Nothing,  -- Lat-lon is unlikely to matter or be available.
    IC.veLastMod = Nothing,
    IC.veLocation =  icTextVal IC.Location <$> dcLocation se,
    IC.veOrganizer = Nothing :: Maybe IC.Organizer,  -- Could populate, but don't plan to publish unless asked.
    IC.vePriority = IC.Priority 0 def,  --  0 means undefined
    IC.veSeq = IC.Sequence 0 def,  -- 0 kinda means "hasn't changed"?
    IC.veStatus = Just . asICStatus . dcStatus $ se,
    IC.veSummary = Just $ icTextVal IC.Summary (dcName se),
    IC.veTransp = IC.Opaque def,
    IC.veUrl = Nothing, --TODO. We should obviously have this, but i think i gotta cobble it together from guild data? 
    IC.veRecurId = Nothing,
    IC.veRRule = mempty, -- :: Set IC.RRule; Recurrence rules.
    IC.veDTEndDuration = Left . (`IC.DTEndDateTime` def) . IC.UTCDateTime <$> dcEndTime se,
    IC.veAttach = fromList . maybeToList $ imageAttachment . eventImageURI (dcId se) <$> dcImage se,
    IC.veAttendee = mempty,
    IC.veCategories = mempty,
    IC.veComment = mempty, -- :: Set IC.Comment,
    IC.veContact = mempty,
    IC.veExDate = mempty, -- :: Set IC.ExDate; exceptions to recurrence rule
    IC.veRStatus = mempty, -- :: Set IC.RequestStatus; I think its for HTTP style interactions?
    IC.veRelated = mempty,
    IC.veResources = mempty,
    IC.veRDate = mempty, -- :: Set IC.RDate; also for recurrences
    IC.veAlarms = mempty, -- of course people should set their own.
    IC.veOther = mempty -- :: Set IC.OtherProperty
  }

{-
X  dcId :: DC.ScheduledEvent -> DP.ScheduledEventId
-  dcGuildId :: DC.ScheduledEvent -> DP.GuildId
X  dcLocation :: DC.ScheduledEvent -> Maybe T.Text
-  dcCreatorId :: DC.ScheduledEvent -> Maybe DP.UserId
X  dcName :: DC.ScheduledEvent -> T.Text
X  dcDescription :: DC.ScheduledEvent -> Maybe T.Text
X  dcStartTime :: DC.ScheduledEvent -> DT.UTCTime
X  dcEndTime :: DC.ScheduledEvent -> Maybe DT.UTCTime
X  dcPrivacyLevel :: DC.ScheduledEvent -> DC.ScheduledEventPrivancyLevel  
X  dcStatus :: DC.ScheduledEvent -> DC.ScheduledEventStatus
-  dcEntityId :: DC.ScheduledEvent -> Maybe DP.ScheduledEventEntityId
-  dcCreator :: DC.ScheduledEvent -> Maybe DT.User
-  dcUserCount :: DC.ScheduledEvent -> Maybe Integer
X  dcImage :: DC.ScheduledEvent -> Maybe DC.ScheduledEventImageHash
 - -}

makeUID :: DC.ScheduledEvent -> TL.Text
makeUID se = (TL.pack . dcShowId . dcId) se <> "@discord"

dcShowId :: DP.ScheduledEventId -> String
dcShowId = show @Word64 . DP.unSnowflake . DP.unId

dcIdSText :: DC.ScheduledEvent -> TS.Text
dcIdSText = TS.pack . dcShowId . dcId

pretty :: ICalendar -> TL.Text
pretty = TL.pack . show

dcId :: DC.ScheduledEvent -> DP.ScheduledEventId
dcId DC.ScheduledEventStage{DC.scheduledEventStageId=x} = x
dcId DC.ScheduledEventVoice{DC.scheduledEventVoiceId=x} = x
dcId DC.ScheduledEventExternal{DC.scheduledEventExternalId=x} = x

dcGuildId :: DC.ScheduledEvent -> DP.GuildId
dcGuildId DC.ScheduledEventStage{DC.scheduledEventStageGuildId=x} = x
dcGuildId DC.ScheduledEventVoice{DC.scheduledEventVoiceGuildId=x} = x
dcGuildId DC.ScheduledEventExternal{DC.scheduledEventExternalGuildId=x} = x

dcLocation :: DC.ScheduledEvent -> Maybe TL.Text
dcLocation DC.ScheduledEventExternal{DC.scheduledEventExternalLocation=x} = Just $ TL.fromStrict x
dcLocation _ = Nothing

dcCreatorId :: DC.ScheduledEvent -> Maybe DP.UserId
dcCreatorId DC.ScheduledEventStage{DC.scheduledEventStageCreatorId=x} = x
dcCreatorId DC.ScheduledEventVoice{DC.scheduledEventVoiceCreatorId=x} = x
dcCreatorId DC.ScheduledEventExternal{DC.scheduledEventExternalCreatorId=x} = x

dcName :: DC.ScheduledEvent -> TL.Text
dcName DC.ScheduledEventStage{DC.scheduledEventStageName=x} = TL.fromStrict x
dcName DC.ScheduledEventVoice{DC.scheduledEventVoiceName=x} = TL.fromStrict x
dcName DC.ScheduledEventExternal{DC.scheduledEventExternalName=x} = TL.fromStrict x

dcDescription :: DC.ScheduledEvent -> Maybe TL.Text
dcDescription DC.ScheduledEventStage{DC.scheduledEventStageDescription=x} = TL.fromStrict <$> x
dcDescription DC.ScheduledEventVoice{DC.scheduledEventVoiceDescription=x} = TL.fromStrict <$> x
dcDescription DC.ScheduledEventExternal{DC.scheduledEventExternalDescription=x} = TL.fromStrict <$> x

dcStartTime :: DC.ScheduledEvent -> DT.UTCTime
dcStartTime DC.ScheduledEventStage{DC.scheduledEventStageStartTime=x} = x
dcStartTime DC.ScheduledEventVoice{DC.scheduledEventVoiceStartTime=x} = x
dcStartTime DC.ScheduledEventExternal{DC.scheduledEventExternalStartTime=x} = x

dcEndTime :: DC.ScheduledEvent -> Maybe DT.UTCTime
dcEndTime DC.ScheduledEventStage{DC.scheduledEventStageEndTime=x} = x
dcEndTime DC.ScheduledEventVoice{DC.scheduledEventVoiceEndTime=x} = x
dcEndTime DC.ScheduledEventExternal{DC.scheduledEventExternalEndTime=x} = Just x

dcPrivacyLevel :: DC.ScheduledEvent -> DC.ScheduledEventPrivacyLevel
dcPrivacyLevel DC.ScheduledEventStage{DC.scheduledEventStagePrivacyLevel=x} = x
dcPrivacyLevel DC.ScheduledEventVoice{DC.scheduledEventVoicePrivacyLevel=x} = x
dcPrivacyLevel DC.ScheduledEventExternal{DC.scheduledEventExternalPrivacyLevel=x} = x

dcStatus :: DC.ScheduledEvent -> DC.ScheduledEventStatus
dcStatus DC.ScheduledEventStage{DC.scheduledEventStageStatus=x} = x
dcStatus DC.ScheduledEventVoice{DC.scheduledEventVoiceStatus=x} = x
dcStatus DC.ScheduledEventExternal{DC.scheduledEventExternalStatus=x} = x

dcEntityId :: DC.ScheduledEvent -> Maybe DP.ScheduledEventEntityId
dcEntityId DC.ScheduledEventStage{DC.scheduledEventStageEntityId=x} = x
dcEntityId DC.ScheduledEventVoice{DC.scheduledEventVoiceEntityId=x} = x
dcEntityId DC.ScheduledEventExternal{DC.scheduledEventExternalEntityId=x} = x

dcCreator :: DC.ScheduledEvent -> Maybe DT.User
dcCreator DC.ScheduledEventStage{DC.scheduledEventStageCreator=x} = x
dcCreator DC.ScheduledEventVoice{DC.scheduledEventVoiceCreator=x} = x
dcCreator DC.ScheduledEventExternal{DC.scheduledEventExternalCreator=x} = x

dcUserCount :: DC.ScheduledEvent -> Maybe Integer
dcUserCount DC.ScheduledEventStage{DC.scheduledEventStageUserCount=x} = x
dcUserCount DC.ScheduledEventVoice{DC.scheduledEventVoiceUserCount=x} = x
dcUserCount DC.ScheduledEventExternal{DC.scheduledEventExternalUserCount=x} = x

dcImage :: DC.ScheduledEvent -> Maybe DC.ScheduledEventImageHash
dcImage DC.ScheduledEventStage{DC.scheduledEventStageImage=x} = x
dcImage DC.ScheduledEventVoice{DC.scheduledEventVoiceImage=x} = x
dcImage DC.ScheduledEventExternal{DC.scheduledEventExternalImage=x} = x

icTextVal :: (TL.Text -> Maybe b -> Maybe c -> IC.OtherParams -> e) -> TL.Text -> e
icTextVal constructor text = constructor text Nothing Nothing def

asICStatus :: DC.ScheduledEventStatus -> IC.EventStatus
asICStatus DC.ScheduledEventStatusScheduled = IC.ConfirmedEvent def
asICStatus DC.ScheduledEventStatusActive = IC.ConfirmedEvent  def
asICStatus DC.ScheduledEventStatusCompleted = IC.ConfirmedEvent  def
asICStatus DC.ScheduledEventStatusCancelled = IC.CancelledEvent def

-- | This is stinky, but it's like this because Discord only has one privacy level.
asICPrivacyClass :: DC.ScheduledEventPrivacyLevel -> IC.Class
asICPrivacyClass DC.ScheduledEventPrivacyLevelGuildOnly = IC.Class IC.Public def

eventImageURI :: DP.ScheduledEventId -> DC.ScheduledEventImageHash -> URI
eventImageURI event hash = URI{
    uriScheme = "https:",
    uriAuthority = Just $ URIAuth{uriUserInfo="", uriRegName="cdn.discordapp.com", uriPort=""},
    uriPath = "/guild-events/" <> dcShowId event <> "/" <> TS.unpack hash <> ".png",
    uriQuery = "",  -- ?size=desired_size to the URL. Image size can be any power of two between 16 and 4096.
    uriFragment = ""
  }

imageAttachment :: URI -> IC.Attachment
imageAttachment uri = IC.UriAttachment (Just $ Image "png") uri def

