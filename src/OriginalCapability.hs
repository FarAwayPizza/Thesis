{-# LANGUAGE GADTs,LambdaCase,ImpredicativeTypes,ScopedTypeVariables #-}

module OriginalCapability where 

import Control.Applicative
import Control.Monad (guard,ap)

-- Original 

-- Vi vil ha:

{-
create :: (forall a. f a -> Eff effs a) -> Eff effs (Capability f)
use :: Capability f -> f a -> Eff effs a
-}


type Capability f = Integer

data CapabilityEffect f a where
  Create :: (forall a. f a -> a) -> CapabilityEffect f (Capability f)
  Use :: Capability f -> f a -> CapabilityEffect f a


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

-- TODO: Oversette til freer-simple
-- runCapabilityEffect kan da implementeres ved hjelp av interpret
runCapabilityEffect :: Eff (CapabilityEffect f :+: effs) a -> Eff (State (CapabilityMap f) :+: effs) a
runCapabilityEffect (Pure x) = Pure x
runCapabilityEffect (Impure (Fmap f (InLeft (Create c)))) = do
                                                              m <- wrap (InLeft Get)
                                                              wrap (InLeft (Put (extend m c)))
                                                              runCapabilityEffect (f (getNext m))
runCapabilityEffect (Impure (Fmap f (InLeft (Use c x)))) = do
                                                              m <- wrap (InLeft Get)
                                                              runCapabilityEffect (f (call m c x)) 
runCapabilityEffect (Impure (Fmap f (InRight x ))) = Impure (Fmap (runCapabilityEffect . f) (InRight x))

-- Eksempel

data ContactCapability a where
   Name :: ContactCapability String
   SendMessage :: String -> ContactCapability ()



example :: Eff (CapabilityEffect ContactCapability) ()
example = do
  me <- create (\x -> case x of
             Name -> "Håkon"
             (SendMessage _) -> ())
  use me (SendMessage "hei")
  pure ()

{- Functor stuff: -}

data (f :+: g) a = InLeft (f a)
                 | InRight (g a)
  deriving (Eq,Show)


instance (Functor f, Functor g) => Functor (f :+: g) where
   fmap h (InLeft x) = InLeft (fmap h x)  -- x :: f a    h :: a -> b     fmap h x :: f b
   fmap h (InRight y) = InRight (fmap h y)

data Identity a = Identity {runIdentity :: a }
   deriving (Eq, Show, Functor)


type f :->: g = forall a. f a -> g a



data FreeF f a where
   Fmap :: (a -> b) -> f a -> FreeF f b

instance Functor (FreeF f) where
   fmap f (Fmap g e) = Fmap (f . g) e

data FreeM f a = Pure a
               | Impure (f (FreeM f a))
  deriving (Functor)


instance (Functor f) => Applicative (FreeM f) where
   pure = Pure
   (<*>) = ap


instance (Functor f) => Monad (FreeM f) where
   (Pure a) >>= f = f a
   (Impure m) >>= f = Impure ((>>= f) <$> m)


type Eff f a = FreeM (FreeF f) a

wrap :: f a -> FreeM (FreeF f) a
wrap = Impure . Fmap Pure 


{-
-- Hos Håkon

  do
     me <- create (\case
                       Name -> pure "Håkon"
                       (SendMessage msg) -> do
                                         inbox <- openFile Append "/home/hakon/inbox"
                                         hPutStrLn inbox msg )
     sendContactToJacob me


-- Hos Jacob

  do
     hakon <- receiveContact
     name <- use hakon Name
     use hakon $ SendMessage $ "Hei " ++ name
-}


