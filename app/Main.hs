module Main where

import App (pingpongExample)
import Arguments (getArguments, MainArgs(..))

main :: IO ()
main = do (MainArgs tok guildid) <- getArguments
          pingpongExample tok guildid

