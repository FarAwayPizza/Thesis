{-# LANGUAGE OverloadedStrings #-}

module Client where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Aeson ()

import GHC.Generics ()
import Servant.Client

import APIs (bankapi)

runBankClient :: IO ()
runBankClient = do
  mgr <- newManager defaultManagerSettings -- creates an HTTP connection manager usd by the client to manage HTTP connections efficiently
  let getAccountId' :<|> getBalance' :<|> createTestAccount :<|> transfer' = client bankapi -- getAccountId'  needs to use one of the given handlers
  _ <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
    testAccount <- createTestAccount
    idT <- getAccountId' testAccount
    liftIO $ print idT
    bT <- getBalance' testAccount
    liftIO $ print bT
    tT <- transfer' testAccount idT bT
    liftIO $ print tT
    return ()
  threadDelay 3000000
  return ()
