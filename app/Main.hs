module Main where

import Arguments (getArguments, MainArgs(..))
import Server (serve)

main :: IO ()
main = do (MainArgs tok guildid port) <- getArguments
          serve port tok guildid

