module Calendar where

import qualified Data.Text as T
import qualified Discord.Internal.Types.ScheduledEvents as DC
import qualified Text.ICalendar as IC

type ICalendar = IC.VCalendar

asICalendar :: DC.ScheduledEvent -> ICalendar
asICalendar se = undefined

pretty :: ICalendar -> T.Text
pretty = T.pack . show
