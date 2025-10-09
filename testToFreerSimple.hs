{-# LANGUAGE GADTs,LambdaCase,ImpredicativeTypes,ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad (guard,ap)
import Control.Monad.Freer 



-- Vi vil ha:

{-
create :: (forall a. f a -> Eff effs a) -> Eff effs (Capability f)
use :: Capability f -> f a -> Eff effs a
-}


type Capability f = Integer

data CapabilityEffect f a where 
  Create :: (forall a. f a -> Eff effs a) -> Eff effs (Capability f) 
  Use :: Capability f -> f a -> Eff effs a





create :: (forall a. f a -> a) -> Eff (CapabilityEffect f) (Capability f)
create f = wrap (Create f)

use c = wrap . Use c


newtype CapabilityMap f = CapabilityMap (Integer,Integer -> (forall a. f a -> a)) 

getNext :: CapabilityMap f -> Capability f
getNext (CapabilityMap (i,_)) = i

extend :: CapabilityMap f -> (forall a. f a -> a) -> CapabilityMap f
extend (CapabilityMap (s, m)) c = CapabilityMap (s+1,\x -> if (x == s) then c else m x)

call :: CapabilityMap f -> Integer -> f a -> a
call (CapabilityMap (_,m)) i x = m i x

data State s a where
  Get :: State s s
  Put :: s -> State s ()

