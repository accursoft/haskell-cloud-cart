import Control.Monad
import Data.Char
import System.IO
import Network
import Network.BSD
import Network.Socket
import System.Environment

main = do [host,port] <- getArgs
          sock <- socket AF_INET Stream defaultProtocol
          setSocketOption sock ReuseAddr 1
          inet <- inet_addr host
          bind sock $ SockAddrInet (fromInteger $ read port) inet
          listen sock maxListenQueue
          forever $ do (handle,_,_) <- Network.accept sock
                       read <- liftM (any (null . dropWhile isSpace) . lines) $ hGetContents handle
                       when read $ hPutStr handle "HTTP/1.1 200 OK\r\n\r\nWelcome to Haskell Cloud"
                       hClose handle
