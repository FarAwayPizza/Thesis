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
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, writeTVar)

-- import Control.Monad.Trans.State -- from the "transformers" library
import Data.Aeson ()
import FreerCapability (
  Capability,
 )
import GHC.Generics ()
import Network.Wai.Handler.Warp ()
import Servant

-- Forklaring på hva clienten kan gjøre
type CapabilityAPI =
  Capture "capability" (Capability AccountCapability)
    :> ( "accountId" :> Capture "AccountCapability" (AccountCapability AccountId) :> Get '[JSON] AccountId
           :<|> "balance" :> Capture "balance" (AccountCapability Amount) :> Get '[JSON] Amount
           :<|> "transfer" :> Capture "transfer" (AccountCapability (Either String Amount)) :> Get '[JSON] (Either String Amount)
       )

--         Neste steg lage handler som bruker capability + IO  på server siden
--         Plus en handler på client siden

-- capability tilsvarer iden, må kanskje lage en oppslagstabell. Eller om den bare kan brukes straight up.
--
server :: TVar BankState -> Server CapabilityAPI
server bankRef capability =
  getAccountId bankRef capability
    :<|> getBalance bankRef capability
    :<|> getTransfer bankRef capability

-- Skal gjøre det samme som createAccount i BankCapability

getAccountId :: TVar BankState -> Capability AccountCapability -> AccountCapability AccountId -> Handler AccountId
getAccountId bankRef capId accountCap = undefined

--  bank <- liftIO $ atomically $ readTVar bankRef
--  pure (Map.lookup accountId bank)

-- How do I receive the capability argument from the client request?
-- How do I extract the account id from the capability?
-- How do I return it as Jason ?

getBalance :: TVar BankState -> Capability AccountCapability -> AccountCapability Amount -> Handler Amount
getBalance _bankRef _cap _balanceCap = undefined

getTransfer :: TVar BankState -> Capability AccountCapability -> AccountCapability (Either String Amount) -> Handler (Either String Amount)
getTransfer _bankRef _cap _transferCap = undefined
