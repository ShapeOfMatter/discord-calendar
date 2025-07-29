module MyDiscord where

import qualified Data.Text as T
import           Discord ( DiscordHandler
                         , restCall)
import           Discord.Types ( Channel(..)
                               , ChannelId
                               , GuildId
                               , Message
                               , messageAuthor
                               , messageContent
                               , userIsBot)
import qualified Discord.Requests as R
import UnliftIO (throwString)


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
