{-# LANGUAGE GADTs, DataKinds, DeriveGeneric, OverloadedStrings #-}

module BankCapability where 

import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad.Freer 
import Control.Concurrent.STM (atomically, newTVar, TVar)
import Control.Monad 
import FreerCapability
  ( CapabilityEffect
  , Capability
  , create
  , use
  , CapabilityMap(CapabilityMap)
  , runCapabilityEffect 
  , runCapabilityEffectSTM 
  )

type InitialBalance = Integer 
type Amount = Integer 
type AccountId = Integer


{- Change String to be an type BankErrors with different ErrorMessageResponses. -}
data BankCapability a where 
  CreateAccount :: String -> InitialBalance -> BankCapability AccountId 
  GetBalance :: AccountId -> BankCapability Amount  
  Transfer :: AccountId -> AccountId -> Amount -> BankCapability (Either String Amount)   
  Deposit ::  AccountId -> Amount -> BankCapability (Either String Amount)
  Withdraw :: AccountId -> Amount -> BankCapability (Either String Amount)

instance Show (BankCapability a) where
  show (CreateAccount name balance) = "CreateAccount " ++ name ++ " " ++ show balance 
  show (GetBalance accId) = "GetBalance " ++ show accId
  show (Transfer from to amount)  = "Transfer " ++ show from ++ " -> " ++ show to ++ " " ++ show amount ++ "$"
  show (Deposit accId amount) = "Deposit " ++ show amount ++ " to " ++ show accId
  show (Withdraw accId amount) = "Withdraw " ++ show amount ++ " from " ++ show accId 

instance ToJSON (BankCapability a) where 
 toJSON (CreateAccount name balance) = object ["action" .= ("CreateAccount" :: String), "name" .= name, "balance" .= balance]
 toJSON (GetBalance accId) = object ["action" .= ("GetBalance" :: String), "accId" .= accId] 
 toJSON (Transfer from to amount) = object ["action" .= ("Transfer" :: String), "from" .= from, "to" .= to, "amount" .= amount]
 toJSON (Deposit accId amount) = object ["action" .= ("Deposit" :: String), "accId" .= accId, "amount" .= amount]
 toJSON (Withdraw accId amount) = object ["action" .= ("Withdraw" :: String), "accId" .= accId, "amount" .= amount]


{-
instance FromJSON (BankCapability AccountId) where 
  parseJSON = withObject "BankCapability" $ \v -> CreateAccount <$> (do
                                                                  x <- v.: "action"
                                                                  guard $ x == ("CreateAccount" :: String)
                                                                  v.: "message",
                                                                  )
-}

instance FromJSON (BankCapability AccountId) where
  parseJSON = withObject "BankCapability" $ \v -> do
                                                   action <- v .: "action"
                                                   case action :: String of
                                                    "CreateAccount" -> CreateAccount <$> v .: "name" <*> v .: "balance"
                                                    _ -> fail "Invalid action for AccountId"






data BankActions 
 = CreateAccountAction Integer String InitialBalance 
 | ShowBalance Integer AccountId
 | ApproveTransfer Integer AccountId AccountId Amount
 | ApproveDeposit Integer AccountId Amount
 | ApproveWithdrawal Integer AccountId Amount
 deriving (Show, Generic) 

instance ToJSON BankActions
instance FromJSON BankActions


data BankResponse 
  = AccountCreatedResponse String 
  | ErrorMessageResponse String 
  | GetBalanceResponse Integer  
  | TransferSuccessfulResponse String 
  | DepositSuccessfulResponse String 
  | WithdrawalSuccessful String  
  deriving (Show, Generic)

instance ToJSON BankResponse
instance FromJSON BankResponse


