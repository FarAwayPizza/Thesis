{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module APIs where

import BankCapability ()
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Servant.Client

import BankCapability
import Control.Monad.Freer hiding (run)
import Data.Aeson ()
import FreerCapability (
  Capability,
  CapabilityEffect,
  CapabilityMap,
  emptyCapabilityMap,
  runCapabilityEffectSTM',
  use,
 )
import GHC.Generics ()
import Network.Wai.Handler.Warp (run)
import Servant

-- Forklaring på hva clienten kan gjøre
type BankAPI =
  "getAccountId" :> ReqBody '[JSON] (Capability AccountCapability) :> Get '[JSON] AccountId
    :<|> "getBalance" :> ReqBody '[JSON] (Capability AccountCapability) :> Get '[JSON] Amount
    :<|> "createTestAccount" :> Get '[JSON] (Capability AccountCapability)
    :<|> "transfer"
      :> ReqBody '[JSON] (Capability AccountCapability)
      :> ReqBody '[JSON] AccountId
      :> ReqBody '[JSON] Amount
      :> Post '[JSON] (Either String Amount)

bankapi :: Proxy BankAPI
bankapi = Proxy

runBankServer :: IO ()
runBankServer = do
  let port = 8080
  mgr <- newManager defaultManagerSettings
  capMap <- newTVarIO emptyCapabilityMap
  readTVarIO capMap >>= print
  bankRef <- newTVarIO Map.empty
  readTVarIO capMap >>= print
  let runApp = run port (bankApp bankRef capMap)
  bracket (forkIO runApp) killThread $ \_ -> do
    let getAccountId' :<|> getBalance' :<|> createTestAccount :<|> transfer' = client bankapi
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
