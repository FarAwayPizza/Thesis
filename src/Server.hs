{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.Freer hiding (run)
import Control.Monad.Freer.Reader
import Control.Monad.IO.Class
import Data.Map (Map)
import Data.Map qualified as Map
import Network.Wai.Handler.Warp (run)
import Servant

import APIs
import BankCapability
import FreerCapability (Capability, CapabilityEffect, CapabilityMap, emptyCapabilityMap, runCapabilityEffectSTM', use)

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
  let newId =
        case Map.keys bank of
          [] -> AccountId 0
          ks -> let AccountId maxId = maximum ks in AccountId (maxId + 1)
  -- let AccountId maxId = maximum (Map.keys bank)
  raise $ createAccount bankRef newId

bankApp :: TVar (Map AccountId Amount) -> TVar (CapabilityMap '[IO] AccountCapability) -> Application
bankApp bank s = serve bankapi $ hoistServer bankapi (liftIO . runM . runCapabilityEffectSTM' s . runReader bank) bankServer

runBankServer :: IO ()
runBankServer = do
  let port = 8080

  capMap <- newTVarIO emptyCapabilityMap
  bankRef <- newTVarIO Map.empty

  putStrLn ("Bank server runnin on port " ++ show port)

  run port (bankApp bankRef capMap)
