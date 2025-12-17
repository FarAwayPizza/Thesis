


{-# LANGUAGE GADTs, LambdaCase, TypeOperators, DataKinds, FlexibleContexts, ImpredicativeTypes, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
module FreerCapability where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Concurrent.STM

type Capability f = Integer

data CapabilityEffect effs f a where
  Create :: (forall a. f a -> Eff effs a) -> CapabilityEffect effs f (Capability f)
  Use :: Capability f -> f a -> CapabilityEffect effs f a


data ReceiveCapability c a where 
  Receive :: ReceiveCapability c (Capability c) 


connect :: IO (forall a. c a -> IO a) -> Eff(CapabilityEffect '[IO] (ReceiveCapability c) ': IO ': '[] ) (Capability (ReceiveCapability c)) 
connect service =  create @'[IO] $ \c -> case c of 
   Receive -> do 
       handlerFn <- (send service); 
       let innerHandler x  = send (handlerFn x)
       create @'[IO] innerHandler
                                                                                                    
                                                                                                   
--  Bank Example (I BankCapability) we need: createBankService :: IO (IO (AccountCapability a -> IO a))
-- createBankService(skal lages i BankCapability ) createa a new TVar with the bank map, and forks a thread to handle requests to create a accounts and handle
-- account actions 

create :: forall effs0 effs f.  Member (CapabilityEffect effs0 f) effs => (forall a. f a -> Eff effs0 a) -> Eff effs (Capability f)
create handler = send (Create @f @effs0 handler)

use :: forall effs0 effs f a.  Member (CapabilityEffect effs0 f) effs => Capability f -> f a -> Eff effs a
use cap eff = send (Use @f @a @effs0 cap eff)


newtype CapabilityMap effs f = CapabilityMap (Integer, Integer -> (forall a. f a -> Eff effs a))
emptyCapabilityMap = CapabilityMap (0, \ x -> undefined ) 

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


runCapabilityEffectSTM :: TVar (CapabilityMap effs f)  
                        -> Eff (CapabilityEffect effs f ': effs) a 
                        -> Eff (IO ': effs ) a  
runCapabilityEffectSTM capMapTVar = reinterpret $ \case
  Create handler -> do 
   m <- send $ atomically $ do 
    currentMap <- readTVar capMapTVar 
    let extendedMap = extend currentMap handler 
    writeTVar capMapTVar extendedMap
    return extendedMap
   return (getNext m) 

  Use cap eff -> do  
   m <- send $ atomically $ readTVar capMapTVar
   raise $ call m cap eff




