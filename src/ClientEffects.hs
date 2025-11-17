
{-# LANGUAGE DataKinds, GADTs #-}

module ClientEffects where  


import Control.Monad.Freer
import Prelude hiding (read) 
import Network.Socket hiding (accept)
import qualified Network.Socket as Socket 
import System.IO  
import Control.Concurrent (forkIO)

-- A server talks to many  clients 
data Server client a where 
  Accept :: Server client client
  ReadFrom :: client -> Server client String --Read from spesific client
  WriteTo :: client -> String -> Server client () -- Write to spesific client 

-- A client only needs to talk to one server 
data Client a where 
  Read :: Client String  --Read from THE server 
  Write :: String -> Client () --Read from THE client 

accept :: Member (Server String) effs => Eff effs String 
accept = send Accept

readFrom :: Member (Server String) effs => String -> Eff effs String  
readFrom c = send (ReadFrom c)

writeTo :: Member (Server String) effs => String -> String -> Eff effs ()
writeTo c s = send (WriteTo c s)


read :: Member Client effs => Eff effs String   
read = send Read 

write :: Member Client effs => String -> Eff effs ()  
write s = send (Write s)  

example :: Eff '[Client, IO] ()  
example = do 
  write "hello" 
  x <- read
  send $ putStrLn x 

exampleServer :: Eff '[Server String, IO] ()
exampleServer = do
    c <- accept
    str <- readFrom c
    writeTo c (reverse str)

runClientSimple :: Eff '[Client, IO] a -> Eff '[IO] a 
runClientSimple = interpret f where 
  f :: Client ~> Eff '[IO] 
  f Read = send getLine 
  f (Write s) = send (putStrLn s) 

runServerSimple :: Eff '[Server String, IO] a -> Eff '[IO] a 
runServerSimple = interpret f where 
  f :: Server String ~> Eff '[IO] 
  f Accept = return "client_1"
  f (ReadFrom clientId) = send getLine  
  f (WriteTo clientId msg) = send (putStrLn msg)

  
runClientTCP :: Member IO effs => Eff (Client ': effs) a -> Eff effs a 
runClientTCP action = do 
  addr <- send $ head <$> getAddrInfo (Just (defaultHints { addrSocketType = Stream})) (Just "127.0.0.1") (Just "1234")
  socket <- send$ (openSocket addr) 
  send$ connect socket $ addrAddress addr 
  hcon <- send$ socketToHandle socket ReadWriteMode 

  x <- interpret (f hcon) action 
  send $ close socket 
  return x where
    f :: Member IO effs => Handle -> Client ~> Eff effs 
    f h  Read = send $ hGetLine h  
    f h (Write s) = send $ hPutStrLn h s  


runServerTCP :: Member IO effs => Eff (Server String ': effs) a -> Eff effs a 
runServerTCP action = do
  addr <- send $ head <$> getAddrInfo (Just (defaultHints {addrSocketType = Stream})) (Just "127.0.0.1") (Just "1234")
  socket <- send $ openSocket addr 
  send $ bind socket (addrAddress addr)
  send $ listen socket 1024
  
  (clientSocket, _) <- send $ Socket.accept socket 
  hcon <- send $ socketToHandle clientSocket ReadWriteMode

  result <- interpret (serverInterpreter hcon) action
  send $ close clientSocket
  send $ close socket 
  return result
  where
    serverInterpreter :: Member IO effs => Handle -> Server String ~> Eff effs  
    serverInterpreter h Accept = return "client_1" 
    serverInterpreter h (ReadFrom clientId) = send $ hGetLine h 
    serverInterpreter h (WriteTo clientId msg) = send $ hPutStrLn h msg


runServerTCPSequential :: Member IO effs => Eff (Server String ': effs) a -> Eff effs ()
runServerTCPSequential action = do
  send $ putStrLn "Starting sequential server on port 1234..."
  addr <- send $ head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just "1234")
  socket <- send $ openSocket addr
  send $ bind socket (addrAddress addr)
  send $ listen socket 1024
  send $ putStrLn "Server ready! Waiting for clients..."
  

  let serverLoop clientNumber = do
        send $ putStrLn $ "Waiting for client #" ++ show clientNumber ++ "..."
        (clientSocket, clientAddr) <- send $ Socket.accept socket
        send $ putStrLn $ "Client #" ++ show clientNumber ++ " connected from: " ++ show clientAddr
        hcon <- send $ socketToHandle clientSocket ReadWriteMode
        result <- interpret (sequentialServerInterpreter hcon clientNumber) action
        send $ close clientSocket
        send $ putStrLn $ "Client #" ++ show clientNumber ++ " finished with: " ++ "finished"

        serverLoop (clientNumber + 1)
  
  serverLoop 1
  where
    sequentialServerInterpreter :: Member IO effs => Handle -> Int -> Server String ~> Eff effs
    sequentialServerInterpreter h clientNum Accept = return $ "client_" ++ show clientNum
    sequentialServerInterpreter h clientNum (ReadFrom clientId) = send $ hGetLine h
    sequentialServerInterpreter h clientNum (WriteTo clientId msg) = do
      send $ hPutStrLn h msg
      send $ hFlush h


