{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module APIs where 

import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

type SimpleAPI name res resId = 
      name :> 
        ( Get '[JSON] [res] 
        :<|> Capture "id" resId :> Get '[JSON] res 
        ) 

-- Forklaring på hva clienten kan gjøre 
-- API = Capture "capability" (Capability AcccountCapability)
--     :> (     "accountId" :> Capture (AcccountCapability AccountId) :> Get '[JSON] AccountId
--         :<|> "balance" :> Capture (AcccountCapability Amount) :> Get ' [JSON] Amount
--         :<|> "transfer" :> Capture (AcccountCapability (Either String Amount)) :> Get '[JSON] (Either String Amount)
--         )
--
--         Neste steg lage handler som bruker capability + IO  på server siden 
--         Plus en handler på client siden 
--        

type API = BankAPI 
      :<|> SimpleAPI "accounts" Account AccountId 
      :<|> SimpleAPI "balance" Balance Sum 
      :<|> SimpleAPI "transfer" Transfer TransferId 


-- taken from the Servant documentation 
type FactoringAPI =
  "x" :> Capture "x" Int :>
      (    QueryParam "y" Int :> Get '[JSON] Int
      :<|>                       Post '[JSON] Int
      )

-- GetAccountId - get's your own AccountId
-- Handler [Account] 

-- GetBalance - get's your own balance
--  toJSON GetBalance = object ["action" .= ("GetBalance" :: String)]

-- Transfer - Checks if you have enough money. If you do: Take money from your
--  own account and gives it to someone else.
--   toJSON (Transfer to amount) = object ["action" .= ("Transfer" :: String), "to" .= to, "amount" .= amount]

