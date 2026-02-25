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
type CapabilityAPI =
  Capture "capability" (Capability AccountCapability)
    :> ( "accountId" :> Capture "AccountCapability" (AccountCapability AccountId) :> Get '[JSON] AccountId
           :<|> "balance" :> Capture "balance" (AccountCapability Amount) :> Get '[JSON] Amount
           :<|> "transfer" :> Capture "transfer" (AccountCapability (Either String Amount)) :> Get '[JSON] (Either String Amount)
       )

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
type BankM = Eff (Reader (TVar (Map AccountId Amount)) ': CapabilityEffect '[IO] AccountCapability ': IO ': '[])

bankServer :: ServerT BankAPI BankM
bankServer =
  handleGetAccountID
    :<|> handleGetBalance
    :<|> handleCreateTestAccount
    :<|> handleTransfer

handleGetAccountID :: Capability AccountCapability -> BankM AccountId
handleGetAccountID account = use @'[IO] account GetAccountId

handleGetBalance :: Capability AccountCapability -> BankM Amount
handleGetBalance account = use @'[IO] account GetBalance

handleTransfer :: Capability AccountCapability -> AccountId -> Amount -> BankM (Either String Amount)
handleTransfer account accId amount = use @'[IO] account (Transfer accId amount)

handleCreateTestAccount :: BankM (Capability AccountCapability)
handleCreateTestAccount = do
  bankRef <- ask
  bank <- send $ readTVarIO bankRef
  let AccountId maxId = maximum (Map.keys bank)
  raise $ createAccount bankRef (AccountId (maxId + 1))

bankApp :: TVar (Map AccountId Amount) -> TVar (CapabilityMap '[IO] AccountCapability) -> Application
bankApp bank s = serve bankapi $ hoistServer bankapi (liftIO . runM . runCapabilityEffectSTM' s . runReader bank) bankServer

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
      tT <- transfer' testAccount
      liftIO $ print tT
      return ()
    threadDelay 3000000
    return ()
