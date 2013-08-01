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
          (env,_) <- unzip `liftM` getEnvironment
          let response = "HTTP/1.1 200 OK\r\n\r\nWelcome to Haskell Cloud!\n\n" ++ unlines (zipWith
                           (\var db -> db ++ if var `elem` env then " is installed on $" ++ var ++ "." else " is not installed.")
                           ["OPENSHIFT_MYSQL_DB_URL", "OPENSHIFT_MONGODB_DB_URL", "OPENSHIFT_POSTGRESQL_DB_URL"]
                           ["MySQL", "MongoDB", "PostgreSQL"]) ++
                         "\nIf you have installed a database that is not recognised, restart the Haskell cartridge."
          forever $ do (handle,_,_) <- Network.accept sock
                       read <- liftM (any (null . dropWhile isSpace) . lines) $ hGetContents handle
                       when read $ hPutStr handle response
                       hClose handle
