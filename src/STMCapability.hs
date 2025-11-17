
{-# LANGUAGE GADTs, ImpredicativeTypes   #-}
module STMCapability where 

import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent.STM
import Control.Monad


type Capability f = Integer 

newtype CapabilityMap f = CapabilityMap (Integer, Integer -> (forall a. f a -> a))


getNext :: CapabilityMap f -> Capability f
getNext (CapabilityMap (i,_)) = i

extend :: CapabilityMap f -> (forall a. f a -> a) -> CapabilityMap f
extend (CapabilityMap (s, m)) c = CapabilityMap (s+1, \x -> if x == s then c else m x)

call :: CapabilityMap f -> Integer -> f a -> a
call (CapabilityMap (_,m)) i x = m i x


createCapability :: TVar (CapabilityMap f) -> (forall a. f a -> a) -> STM (Capability f) 
createCapability mapRef handler = do 
    m <- readTVar mapRef
    writeTVar mapRef (extend m handler)
    return (getNext m) 


useCapability :: TVar (CapabilityMap f) -> Capability f -> f a -> STM a 
useCapability mapRef cap eff = do 
    m <- readTVar mapRef
    return (call m cap eff)


initialMap :: CapabilityMap TestEffect
initialMap = CapabilityMap (0, \_ -> error "Capability not found")

data TestEffect a where
    GetInt :: TestEffect Int
    GetString :: TestEffect String

simpleHandler :: TestEffect a -> a
simpleHandler GetInt = 42
simpleHandler GetString = "hello"



