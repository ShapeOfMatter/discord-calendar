module Main where

import Arguments (getArguments, MainArgs(..))
import Server (serve)

main :: IO ()
main = do (MainArgs tok port) <- getArguments
          serve port tok

