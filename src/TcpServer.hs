
module TcpServer(runServer, handleClient) where 

import Network.Socket
import Control.Concurrent
import qualified Control.Exception as E
import System.IO
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar ()

-- Decide what to to with "tid" on line 32. Added "tid <- " to get rid of purple warning message

handleClient :: Handle -> TVar String -> IO ()
handleClient con motd =
  do
    hello <- readTVarIO motd
    hPutStrLn con hello
    line <- hGetLine con
    atomically $ modifyTVar motd (++ line ++ "\n")

runServer :: IO b 
runServer = do
   motd <- newTVarIO "Hello\n"
   E.bracket open close (loop motd) where
      open = do
            addr <- head <$> getAddrInfo (Just defaultHints) Nothing (Just "1234")
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            bind sock $ addrAddress addr
            listen sock 1024
            return sock
      loop motd sock = do
        (con,_) <- accept sock
        putStrLn "Someone connected"
        hcon <- socketToHandle con ReadWriteMode
        _tid <- forkIO (handleClient hcon motd)
        -- maybe do something with tid later 
        loop motd sock


