{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module BankCapability where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVar, writeTVar)
import Control.Monad ()
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as Map

import FreerCapability (
  Capability,
  CapabilityEffect,
  create,
  emptyCapabilityMap,
  runCapabilityEffect,
  use,
 )
import GHC.Generics ()

newtype InitialBalance = InitialBalance Integer deriving (Show, ToJSON, FromJSON)
newtype Amount = Amount Integer deriving (Num, Eq, Ord, Show, ToJSON, FromJSON)
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
  show GetAccountId = "GetAccountId"
  show GetBalance = "GetBalance"
  show (Transfer to amount) = "Transfer " ++ " -> " ++ show to ++ " " ++ show amount ++ "$"

instance ToJSON (AccountCapability a) where
  toJSON GetAccountId = object ["action" .= ("GetAccountId" :: String)]
  toJSON GetBalance = object ["action" .= ("GetBalance" :: String)]
  toJSON (Transfer to amount) = object ["action" .= ("Transfer" :: String), "to" .= to, "amount" .= amount]

instance FromJSON SomeAccountCapability where
  parseJSON = withObject "AccountCapability" $ \v -> do
    action <- v .: "action"
    case action :: String of
      "GetAccountId" -> pure $ SomeAccountCapability $ GetAccountId
      "GetBalance" -> pure $ SomeAccountCapability $ GetBalance
      "Transfer" -> SomeAccountCapability <$> (Transfer <$> v .: "to" <*> v .: "amount")
      _ -> fail $ "Invalid action for Amount: " ++ action

createAccount :: TVar (Map AccountId Amount) -> AccountId -> Eff (CapabilityEffect '[IO] AccountCapability ': IO ': '[]) (Capability AccountCapability)
createAccount bankRef accountId = do
  send $ atomically $ modifyTVar bankRef (Map.insert accountId (Amount 150))
  create @'[IO] $ \c -> case c of
    GetBalance -> do
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
    GetAccountId -> do
      send $ putStrLn $ "Your accountId is " ++ show accountId
      pure accountId
    Transfer destId amount -> do
      send . atomically $ do
        bank <- readTVar bankRef
        let srcBal = Map.lookup accountId bank
        let dstBal = Map.lookup destId bank
        case (srcBal, dstBal) of
          (Just s, Just d) -> do
            if s >= amount
              then do
                let newSourceBalance = s - amount
                let newDestinationBalance = d + amount
                writeTVar bankRef $
                  Map.insert accountId newSourceBalance $
                    Map.insert destId newDestinationBalance bank
                pure $ Right newSourceBalance
              else pure $ Left "Not sufficient funds"
          _ -> pure $ Left "Account not found"

createBankService :: IO (IO (AccountCapability a -> IO a))
createBankService = undefined

-- bankStateTVar <- atomically $ newTVar Map.empty

type BankTest = CapabilityEffect '[IO] AccountCapability ': IO ': '[]

accountTest :: Eff BankTest ()
accountTest = do
  bankRef <- send $ newTVarIO Map.empty
  let id0 = AccountId 0
  let id1 = AccountId 1
  let id2 = AccountId 2
  testAccount0 <- createAccount bankRef id0
  testAccount1 <- createAccount bankRef id1
  testAccount3 <- createAccount bankRef id2
  _ <- use @'[IO] testAccount0 $ GetBalance
  _ <- use @'[IO] testAccount1 $ GetBalance
  _ <- use @'[IO] testAccount3 $ GetAccountId
  _ <- use @'[IO] testAccount1 $ Transfer id2 (Amount 50)
  _ <- use @'[IO] testAccount1 $ GetBalance
  _ <- use @'[IO] testAccount3 $ GetBalance

  return ()

runAccountTest :: IO ()
runAccountTest = runM $ evalState emptyCapabilityMap (runCapabilityEffect accountTest)
