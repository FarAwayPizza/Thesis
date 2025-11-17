{-# LANGUAGE OverloadedStrings #-}

module TcpClient (runClient) where

import qualified Data.ByteString.Char8 as C
import Network.Run.TCP (runTCPClient)
import Network.Socket.ByteString (recv, sendAll)

runClient :: IO ()
runClient = runTCPClient "127.0.0.1" "1234" $ \s -> do 
  sendAll s "Hello, world!\n"
  msg <- recv s 1024
  putStr "Received: "
  C.putStrLn msg


