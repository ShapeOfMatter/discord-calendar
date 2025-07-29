module Main where

import App (chirpExample)
import Arguments (getArguments, MainArgs(..))

main :: IO ()
main = do (MainArgs tok guildid) <- getArguments
          chirpExample tok guildid

