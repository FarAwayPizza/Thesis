{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ClientEffects where

import Control.Concurrent ()
import Control.Monad.Freer
import Network.Socket hiding (accept)
import Network.Socket qualified as Socket
import System.IO
import Prelude hiding (read)

-- A server talks to many  clients
data Server client a where
  Accept :: Server client client
  ReadFrom :: client -> Server client String -- Read from spesific client
  WriteTo :: client -> String -> Server client () -- Write to spesific client

-- A client only needs to talk to one server
data Client a where
  Read :: Client String -- Read from THE server
  Write :: String -> Client () -- Read from THE client

accept :: (Member (Server String) effs) => Eff effs String
accept = send Accept

readFrom :: (Member (Server String) effs) => String -> Eff effs String
readFrom c = send (ReadFrom c)

writeTo :: (Member (Server String) effs) => String -> String -> Eff effs ()
writeTo c s = send (WriteTo c s)

read :: (Member Client effs) => Eff effs String
read = send Read

write :: (Member Client effs) => String -> Eff effs ()
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
runClientSimple = interpret f
 where
  f :: Client ~> Eff '[IO]
  f Read = send getLine
  f (Write s) = send (putStrLn s)

runServerSimple :: Eff '[Server String, IO] a -> Eff '[IO] a
runServerSimple = interpret f
 where
  f :: Server String ~> Eff '[IO]
  f Accept = return "client_1"
  f (ReadFrom _clientId) = send getLine
  f (WriteTo _clientId msg) = send (putStrLn msg)

runClientTCP :: (Member IO effs) => Eff (Client ': effs) a -> Eff effs a
runClientTCP action = do
  addr <- send $ head <$> getAddrInfo (Just (defaultHints{addrSocketType = Stream})) (Just "127.0.0.1") (Just "1234")
  sock <- send $ (openSocket addr)
  send $ connect sock $ addrAddress addr
  hcon <- send $ socketToHandle sock ReadWriteMode

  x <- interpret (f hcon) action
  send $ close sock
  return x
 where
  f :: (Member IO effs) => Handle -> Client ~> Eff effs
  f h Read = send $ hGetLine h
  f h (Write s) = send $ hPutStrLn h s

runServerTCP :: (Member IO effs) => Eff (Server String ': effs) a -> Eff effs a
runServerTCP action = do
  addr <- send $ head <$> getAddrInfo (Just (defaultHints{addrSocketType = Stream})) (Just "127.0.0.1") (Just "1234")
  sock <- send $ openSocket addr
  send $ bind sock (addrAddress addr)
  send $ listen sock 1024

  (clientSocket, _) <- send $ Socket.accept sock
  hcon <- send $ socketToHandle clientSocket ReadWriteMode

  result <- interpret (serverInterpreter hcon) action
  send $ close clientSocket
  send $ close sock
  return result
 where
  serverInterpreter :: (Member IO effs) => Handle -> Server String ~> Eff effs
  serverInterpreter _h Accept = return "client_1"
  serverInterpreter h (ReadFrom _clientId) = send $ hGetLine h
  serverInterpreter h (WriteTo _clientId msg) = send $ hPutStrLn h msg

runServerTCPSequential :: (Member IO effs) => Eff (Server String ': effs) a -> Eff effs ()
runServerTCPSequential action = do
  send $ putStrLn "Starting sequential server on port 1234..."
  addr <- send $ head <$> getAddrInfo (Just defaultHints{addrSocketType = Stream}) (Just "127.0.0.1") (Just "1234")
  sock <- send $ openSocket addr
  send $ bind sock (addrAddress addr)
  send $ listen sock 1024
  send $ putStrLn "Server ready! Waiting for clients..."

  let serverLoop clientNumber = do
        send $ putStrLn $ "Waiting for client #" ++ show clientNumber ++ "..."
        (clientSocket, clientAddr) <- send $ Socket.accept sock
        send $ putStrLn $ "Client #" ++ show clientNumber ++ " connected from: " ++ show clientAddr
        hcon <- send $ socketToHandle clientSocket ReadWriteMode
        _result <- interpret (sequentialServerInterpreter hcon clientNumber) action
        send $ close clientSocket
        send $ putStrLn $ "Client #" ++ show clientNumber ++ " finished with: " ++ "finished"

        serverLoop (clientNumber + 1)

  serverLoop 1
 where
  sequentialServerInterpreter :: (Member IO effs) => Handle -> Int -> Server String ~> Eff effs
  sequentialServerInterpreter _h clientNum Accept = return $ "client_" ++ show clientNum
  sequentialServerInterpreter h _clientNum (ReadFrom _clientId) = send $ hGetLine h
  sequentialServerInterpreter h _clientNum (WriteTo _clientId msg) = do
    send $ hPutStrLn h msg
    send $ hFlush h
