{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module APIs where

import BankCapability ()
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Network.HTTP.Client (defaultManagerSettings, newManager)

import BankCapability
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson ()
import Data.Map qualified as Map
import FreerCapability (
  Capability,
  emptyCapabilityMap,
 )
import GHC.Generics ()
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client

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
