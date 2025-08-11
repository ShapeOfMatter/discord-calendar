module Arguments where

import Control.Monad (join)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (Port)
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
import Text.Read (readMaybe)




data MainArgs = MainArgs{ botToken :: T.Text
                        , portToServe :: Port
                        } deriving (Show, Read)

argParser :: Parser (IO MainArgs)
argParser = do
              token <- strOption (short 't' <> metavar "TOKEN" <> help "A token for a bot? DISCOURAGED! (Use a secrets file.)")
              port <- option @Port auto (short 'p' <> metavar "PORT" <> help "The port on which to serve HTTP(S) requests.")
              pure $ pure $ MainArgs token port

fileParser :: Parser (IO MainArgs)
fileParser = do
              file <- strOption (short 'f' <> metavar "FILE" <> help "A file from which to read a MainArgs object.")
              pure $ (maybe (error "Secret config file is malformed.") id) . readMaybe @MainArgs <$> readFile file

getArguments :: IO MainArgs
getArguments = join . execParser $ info (argParser <|> fileParser <**> helper) (progDesc helpMessage)

helpMessage :: String
helpMessage = unlines [
    "Expose events from Discord Servers as an iCalendar file."
  ]

