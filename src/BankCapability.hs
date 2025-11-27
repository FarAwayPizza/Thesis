{-# LANGUAGE GADTs, DataKinds, DeriveGeneric, OverloadedStrings #-}

module BankCapability where 

import GHC.Generics (Generic)
import Data.Aeson
import Control.Monad.Freer 
import Control.Concurrent.STM (atomically, newTVar, newTVarIO, readTVar, TVar, modifyTVar)
import Control.Monad 
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.Freer.State 
import FreerCapability
  ( CapabilityEffect
  , Capability
  , create
  , use
  , CapabilityMap(CapabilityMap)
  , runCapabilityEffect 
  , runCapabilityEffectSTM,
  emptyCapabilityMap
  )

newtype InitialBalance = InitialBalance Integer deriving (Show, ToJSON, FromJSON)
newtype Amount = Amount Integer deriving (Show, ToJSON, FromJSON) 
newtype AccountId = AccountId Integer deriving (Show, Eq, Ord, ToJSON, FromJSON)

type BankState = Map.Map AccountId Amount

{- import qualified Data.ByteString.Lazy.Char8 as B -}

data AccountCapability a where 
  GetAccountId :: AccountCapability AccountId
  GetBalance :: AccountCapability Amount  
  Transfer :: AccountId -> Amount -> AccountCapability (Either String Amount)  

data SomeAccountCapability = forall a. SomeAccountCapability (AccountCapability a)

instance Show SomeAccountCapability where
  show (SomeAccountCapability x) = show x

instance Show (AccountCapability a) where
  show GetAccountId  = "GetAccountId"
  show GetBalance = "GetBalance" 
  show (Transfer to amount)  = "Transfer " ++ " -> " ++ show to ++ " " ++ show amount ++ "$"
  
instance ToJSON (AccountCapability a) where 
 toJSON GetAccountId = object ["action" .= ("GetAccountId" :: String)]
 toJSON GetBalance = object ["action" .= ("GetBalance" :: String)]
 toJSON (Transfer to amount) = object ["action" .= ("Transfer" :: String), "to" .= to, "amount" .= amount]


instance FromJSON SomeAccountCapability where
  parseJSON = withObject "AccountCapability" $ \v -> do 
                                                   action <- v .: "action"
                                                   case action :: String of
                                                    "GetAccountId" -> pure $ SomeAccountCapability $ GetAccountId 
                                                    "GetBalance"   -> pure $ SomeAccountCapability $ GetBalance  
                                                    "Transfer"     -> SomeAccountCapability <$> (Transfer <$> v .: "to" <*> v .: "amount") 
                                                    _              -> fail $ "Invalid action for Amount: " ++ action



createAccount :: TVar(Map AccountId Amount) -> AccountId -> Eff(CapabilityEffect '[IO] AccountCapability ': IO ': '[] ) (Capability AccountCapability)
createAccount bankRef accountId = do  
  send $ atomically $ modifyTVar bankRef (Map.insert accountId (Amount 0))
  create @' [IO] $ \c -> case c of
    GetBalance  -> do
      mAmt <- send . atomically $ do 
        bank <- readTVar bankRef
        pure (Map.lookup accountId bank)
      case mAmt of
        Just amt -> do 
          send $ putStrLn $ show accountId ++ " has " ++ show amt ++ "$"
          pure amt 
        Nothing -> do 
          send $ putStrLn $ "No account found for " ++ show accountId 
          pure (Amount 0) 

type BankTest = CapabilityEffect '[IO] AccountCapability ': IO ': '[]  
     

accountTest :: Eff BankTest () 
accountTest = do 
  bankRef <- send $  newTVarIO Map.empty 
  let id0 = AccountId 0 
  let id1 = AccountId 1 
  testAccount0 <- createAccount bankRef id0
  testAccount1 <- createAccount bankRef id1
  use @'[IO] testAccount0 $ GetBalance  
  use @'[IO] testAccount1 $ GetBalance 
  return ()


runAccountTest = runM $  evalState emptyCapabilityMap (runCapabilityEffect accountTest)
