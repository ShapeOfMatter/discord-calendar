module Arguments where

import Control.Monad (join)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
                        , tlsCert :: ByteString
                        , tlsKey :: ByteString
                        , portToServe :: Port
                        } deriving (Show, Read)

argParser :: Parser (IO MainArgs)
argParser = do
              botToken <- strOption (short 't' <> metavar "TOKEN" <> help "A token for a bot? DISCOURAGED! (Use a secrets file.)")
              portToServe <- option @Port auto (short 'p' <> metavar "PORT" <> help "The port on which to serve HTTP(S) requests.")
              certFile <- strOption (short 'c' <> metavar "FILE" <> help "The TLS certificate file for https.")
              keyFile <- strOption (short 'k' <> metavar "FILE" <> help "The key matching the certificate file.")
              pure do tlsCert <- BS.readFile certFile
                      tlsKey <- BS.readFile keyFile
                      pure $ MainArgs{botToken, tlsCert, tlsKey, portToServe}

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

