{-# LANGUAGE GADTs, DataKinds, DeriveGeneric, OverloadedStrings #-}

module ContactCapability where 


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


data ContactCapability a where
  Name :: ContactCapability String
  SendMessage :: String -> ContactCapability ()
  
instance Show (ContactCapability a) where
  show Name = "Name"
  show (SendMessage msg) = "SendMessage " ++ show msg  

instance ToJSON (ContactCapability a) where 
    toJSON Name = object ["action" .=  ("Name" :: String)] 
    toJSON (SendMessage msg) = object ["action" .= ("SendMessage" :: String) , "message" .= msg] 
  

instance FromJSON (ContactCapability String) where 
    parseJSON = withObject "ContactCapability" $ \v -> Name <$ (do 
                                                                  x <- v.: "action" 
                                                                  guard $ x  == ("Name" :: String))


instance FromJSON (ContactCapability ()) where 
    parseJSON = withObject "ContactCapability" $ \v -> SendMessage  <$> (do 
                                                                  x <- v.: "action" 
                                                                  guard $ x  == ("SendMessage" :: String)
                                                                  v.: "message")

data ContactRequest 
  = GetNameRequest Integer 
  | SendMessageRequest Integer String 
  deriving (Show, Generic)

instance ToJSON ContactRequest 
instance FromJSON ContactRequest 

data ContactResponse
  = NameResponse String  
  | MessageSentResponse 
  | ErrorMessageResponse String 
  deriving (Show, Generic) 

instance ToJSON ContactResponse 
instance FromJSON ContactResponse  


{- There wil be a similar function called createBankAccount in BankCapability.hs. 
 - The bank needs to store the bank accounts. Try to use STM TVar and map. 
 - Just like there is a Name and a SendMessage there needs to be one for eacth BankCapability
 - -}
createMailBox :: String -> Eff (CapabilityEffect '[IO] ContactCapability ': IO ': '[]) (Capability ContactCapability)
createMailBox name = create @'[IO] (\c -> case c of 
    Name -> pure name  
    (SendMessage msg) -> send $ putStrLn msg )

initialMap :: CapabilityMap effs f
initialMap = CapabilityMap (0, \_ -> error "Capability not found")

type ContactTest = CapabilityEffect '[IO] ContactCapability ': IO ': '[]  

data TestEffect a where
    GetInt :: TestEffect Int
    GetString :: TestEffect String


simpleHandler :: TestEffect a -> a
simpleHandler GetInt = 42
simpleHandler GetString = "hello"


mailDeliveryTest :: Eff ContactTest () 
mailDeliveryTest = do 
    m <- createMailBox "jacob"
    use @'[IO] m $ SendMessage "Hallo"


testModulesWork :: IO ()
testModulesWork = do
  putStrLn "Modules compiled and types align correctly!"
  let _ = Name :: ContactCapability String
  let _ = SendMessage :: String -> ContactCapability ()
  putStrLn "All ContactCapability constructors work!"
