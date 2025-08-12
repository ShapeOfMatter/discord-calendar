module Main where

import Arguments (getArguments, MainArgs(..))
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettingsMemory)
import Server (server)

main :: IO ()
main = do args <- getArguments
          serve args



serve ::  MainArgs -> IO ()
serve MainArgs{portToServe, botToken, tlsCert, tlsKey} = do
    putStrLn $ "https://host:" <> show portToServe <> "/"
    let warpconf = setPort portToServe defaultSettings
        tlsconf = tlsSettingsMemory tlsCert tlsKey
    runTLS tlsconf warpconf $ server botToken

