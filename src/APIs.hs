{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module APIs where

import BankCapability (
  AccountCapability (..),
  AccountId (..),
  Amount (..),
  BankState,
 )
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Network.HTTP.Client (defaultManagerSettings, newManager)

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
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
type CapabilityAPI =
  Capture "capability" (Capability AccountCapability)
    :> ( "accountId" :> Capture "AccountCapability" (AccountCapability AccountId) :> Get '[JSON] AccountId
           :<|> "balance" :> Capture "balance" (AccountCapability Amount) :> Get '[JSON] Amount
           :<|> "transfer" :> Capture "transfer" (AccountCapability (Either String Amount)) :> Get '[JSON] (Either String Amount)
       )

type BankAPI =
  "getAccountId" :> ReqBody '[JSON] (Capability AccountCapability) :> Get '[JSON] AccountId

bankapi :: Proxy BankAPI
bankapi = Proxy

--         Neste steg lage handler som bruker capability + IO  på server siden
--         Plus en handler på client siden

-- capability tilsvarer iden, må kanskje lage en oppslagstabell. Eller om den bare kan brukes straight up.
-- {-
---}
--
{-
server :: TVar BankState -> Server CapabilityAPI
server bankRef capability =
  getAccountId bankRef capability
    :<|> getBalance bankRef capability
    :<|> getTransfer bankRef capability
-}
type BankM = Eff (CapabilityEffect '[IO] AccountCapability ': IO ': '[])

bankServer :: ServerT BankAPI BankM
bankServer = handleGetAccountID

handleGetAccountID :: Capability AccountCapability -> BankM AccountId
handleGetAccountID account = use @'[IO] account GetAccountId

bankApp :: TVar (CapabilityMap '[IO] AccountCapability) -> Application
bankApp s = serve bankapi $ hoistServer bankapi (liftIO . runM . runCapabilityEffectSTM' s) bankServer

runBankServer :: IO ()
runBankServer = do
  let port = 8080
  mgr <- newManager defaultManagerSettings
  capMap <- newTVarIO emptyCapabilityMap
  readTVarIO capMap >>= print
  account <- runM $ runCapabilityEffectSTM' capMap $ do
    bankRef <- send $ newTVarIO Map.empty
    let id0 = AccountId 0
    createAccount bankRef id0
  readTVarIO capMap >>= print
  let runApp = run port (bankApp capMap)
  bracket (forkIO runApp) killThread $ \_ -> do
    let getAccountId' = client bankapi
    _ <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      idT <- getAccountId' account
      liftIO $ print idT
      return ()
    threadDelay 3000000
    return ()

-- Skal gjøre det samme som createAccount i BankCapability
{-

getBalance :: TVar BankState -> Capability AccountCapability -> AccountCapability Amount -> Handler Amount
getBalance _bankRef _cap _balanceCap = undefined

getTransfer :: TVar BankState -> Capability AccountCapability -> AccountCapability (Either String Amount) -> Handler (Either String Amount)
getTransfer _bankRef _cap _transferCap = undefined

-}
