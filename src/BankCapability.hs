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

{- newtype vs type ? -}
newtype InitialBalance = InitialBalance Integer deriving (Show, ToJSON, FromJSON)
newtype Amount = Amount Integer deriving (Show, ToJSON, FromJSON) 
newtype AccountId = AccountId Integer deriving (Show, ToJSON, FromJSON)


{- import qualified Data.ByteString.Lazy.Char8 as B -}

data BankCapability a where 
  GetAccountId :: String -> BankCapability AccountId
  GetBalance :: AccountId -> BankCapability Amount  
  Transfer :: AccountId -> AccountId -> Amount -> BankCapability (Either String Amount)  

data SomeBankCapability = forall a. SomeBankCapability (BankCapability a)
  
instance Show (BankCapability a) where
  show (GetAccountId accId) = "GetAccountId " ++ show accId
  show (GetBalance balance) = "GetBalance " ++ show balance
  show (Transfer from to amount)  = "Transfer " ++ show from ++ " -> " ++ show to ++ " " ++ show amount ++ "$"
  
instance ToJSON (BankCapability a) where 
 toJSON (GetAccountId name) = object ["action" .= ("GetAccountId" :: String), "name" .= name]
 toJSON (GetBalance accId) = object ["action" .= ("GetBalance" :: String), "accId" .= accId] 
 toJSON (Transfer from to amount) = object ["action" .= ("Transfer" :: String), "from" .= from, "to" .= to, "amount" .= amount]


{-  I need to double check if it's supposed to be name for GetAccountId, accId for GetBalance and amount for Transfer -}
instance FromJSON SomeBankCapability where
  parseJSON = withObject "BankCapability" $ \v -> do 
                                                   action <- v .: "action"
                                                   case action :: String of
                                                    "GetAccountId" -> SomeBankCapability <$> (GetAccountId <$> v .: "name") 
                                                    "GetBalance"   -> SomeBankCapability <$> (GetBalance <$> v .: "accId")
                                                    "Transfer"     -> SomeBankCapability <$> (Transfer <$> v .: "from" <*> v .: "to" <*> v .: "amount") 
                                                    _              -> fail $ "Invalid action for Amount: " ++ action

{- 
data BankResponse 
  =  ErrorMessageResponse String 
  | GetBalanceResponse Integer  
  | TransferSuccessfulResponse String  
  deriving (Show, Generic)

instance ToJSON BankResponse
instance FromJSON BankResponse

-}
