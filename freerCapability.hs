


{-# LANGUAGE GADTs, LambdaCase, TypeOperators, DataKinds, FlexibleContexts, ImpredicativeTypes, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
module FreerCapability where

import Control.Monad.Freer
import Control.Monad.Freer.State

type Capability f = Integer

data CapabilityEffect effs f a where
  Create :: (forall a. f a -> Eff effs a) -> CapabilityEffect effs f (Capability f)
  Use :: Capability f -> f a -> CapabilityEffect effs f a 


create :: forall effs0 effs f.  Member (CapabilityEffect effs0 f) effs => (forall a. f a -> Eff effs0 a) -> Eff effs (Capability f)
create handler = send (Create @f @effs0 handler)

use :: forall effs0 effs f a.  Member (CapabilityEffect effs0 f) effs => Capability f -> f a -> Eff effs a
use cap eff = send (Use @f @a @effs0 cap eff)


newtype CapabilityMap effs f = CapabilityMap (Integer, Integer -> (forall a. f a -> Eff effs a))

getNext :: CapabilityMap effs f -> Capability f
getNext (CapabilityMap (i,_)) = i

extend :: CapabilityMap effs f -> (forall a. f a -> Eff effs a) -> CapabilityMap effs f
extend (CapabilityMap (s, m)) c = CapabilityMap (s+1, \x -> if x == s then c else m x)

call :: CapabilityMap effs f -> Integer -> f a -> Eff effs a
call (CapabilityMap (_,m)) i x = m i x


runCapabilityEffect :: Eff (CapabilityEffect effs f ': effs) a 
                    -> Eff (State (CapabilityMap effs f) ': effs) a
runCapabilityEffect = reinterpret $ \case
  Create handler -> do
    m <- get
    put (extend m handler)
    return (getNext m)
    
  Use cap eff -> do
    m <- get
    raise $ call m cap eff


data ContactCapability a where
  Name :: ContactCapability String
  SendMessage :: String -> ContactCapability ()

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
