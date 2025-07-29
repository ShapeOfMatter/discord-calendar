module Arguments where

import Control.Monad (join)
import qualified Data.Text as T
import           Discord.Types ( GuildId)
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

getArguments :: IO MainArgs
getArguments = join . execParser $ info (argParser <|> fileParser <**> helper) (progDesc $ unlines ["A ping-pong bot."])

