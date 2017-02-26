{-# LANGUAGE BangPatterns, DeriveGeneric #-}
module Network.Vanguard.Core
( Addr
, addrW8
, renderAddr
, addr
, buildAddress
, Connection(..)
, Queue
, Packet
, PacketQueue
, Injector
, passTo
, newQueue
, readQueue
, CommVar
, newCommVar
, putCommVar
, takeCommVar
, withUnixSocket
, withControlSocket
, withProtectedUDPSocket
, withProtectedBoundUDPSocket
, describeSocket
, decompose
, resolveAddress ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO
                                     , readTQueue, writeTQueue)
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVarIO, putTMVar, takeTMVar )
import qualified Data.ByteString as B
import Data.Word (Word8)
import Control.Monad.Fail (MonadFail(..))

import Text.Read (readMaybe)
import Control.Exception ( bracket, bracket_)
import Network.Socket ( Family(AF_UNIX, AF_INET)
                      , SocketType(Stream, Datagram)
                      , SockAddr(SockAddrUnix, SockAddrInet)
                      , Socket, PortNumber
                      , iNADDR_ANY, aNY_PORT
                      , defaultProtocol, socket, bind, close
                      , getSocketName, hostAddressToTuple, tupleToHostAddress
                      , getAddrInfo, AddrInfo(addrAddress) )
import System.Posix.Files ( removeLink )
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Prelude
import Data.String (String, words)

-- |Addr is a type for holding IP addresses. It is the same as the
-- type from network-house (note, the Show and Read instances are
-- different).
data Addr = Addr {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Addr
instance ToJSON Addr

-- |addrW8 takes four Word8s and uses them to build an Addr
-- e.g. addrW8 1 2 3 4 is the address 1.2.3.4
addrW8 :: Word8 -> Word8 -> Word8 -> Word8 -> Addr
addrW8 !a !b !c !d = Addr a b c d

-- |renderAddr turns an Addr into a string in the way we would
-- normally expect. Mostly useful for debugging and testing.
renderAddr :: Addr -> String
renderAddr (Addr a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

-- |addr takes a String and (tries) to convert it into an Addr
-- inside a Monad. Note that if the string contains Integers that
-- are bigger than 255, they are truncated, that is, the least
-- significant bits are taken. e.g.
-- addr "256.0.0.0" yields (Addr 0 0 0 0).
addr :: (MonadFail m) => String -> m Addr
addr str = do
  let ads = words $ map (\x -> if x == '.' then ' ' else x) str
  case sequence $ map readMaybe ads of
    Just [a,b,c,d] -> let !ad = Addr a b c d in return ad
    _ -> fail "Not a valid IPv4 address"

-- Internally threads have queues of packets which we must process.
-- Utility function for resolving addresses
buildAddress :: Addr -> PortNumber -> SockAddr
buildAddress addr port = let Addr a b c d = addr
                             ad = tupleToHostAddress (a,b,c,d) in
                             SockAddrInet port ad


-- |A connection represents all the information we need to store
-- about a single endpoint we are connecting to. This is also the
-- information we need to provide about ourselves.
data Connection = Connection { virtual_ip :: Addr
                             , external_ip :: Addr
                             , port :: PortNumber } deriving (Generic, Eq, Show)


-- QUEUES :: thread safe polymorphic queues (abstraction layer)

newtype Queue a = Queue (TQueue a)
type Packet = B.ByteString
type PacketQueue = Queue Packet
type Injector = PacketQueue

passTo :: a -> Queue a -> IO ()
passTo x (Queue q) = atomically $ writeTQueue q x

newQueue :: IO (Queue a)
newQueue = fmap Queue newTQueueIO

readQueue :: Queue a -> IO a
readQueue (Queue q) = atomically . readTQueue $ q

-- CommVar :: Blocking threadsafe variables for passing variables
-- between two threads. Use TVars or similar if there is no
-- consistent access order to avoid deadlock etc.

newtype CommVar a = CommVar (TMVar a)

newCommVar :: IO (CommVar a)
newCommVar = fmap CommVar newEmptyTMVarIO

putCommVar :: CommVar a -> a -> IO ()
putCommVar (CommVar v) x = atomically $ putTMVar v x

takeCommVar :: CommVar a -> IO a
takeCommVar (CommVar v) = atomically $ takeTMVar v


-- BRACKETED SOCKET FUNCTIONS :: Allows us to create sockets and bindings
-- that are cleared up in the event of an exception

-- |withUnixSocket opens a new (streaming) unix socket, with the promise
-- that it will be closed in the event of an exception, or when the passed
-- action is finished.
withUnixSocket :: (Socket -> IO a) -> IO a
withUnixSocket = bracket
  (socket AF_UNIX Stream defaultProtocol)
  close

-- |withControlSocket opens a (streaming) unix socket which is bound to
-- a passed path. It promises to remove the binding, and clean up the socket
-- in the event of an exception, or after the action has completed running.
withControlSocket :: String -> (Socket -> IO a) -> IO a
withControlSocket path action = withUnixSocket $ \sock ->
    bracket_ (bind sock $ SockAddrUnix path)
             (removeLink path)
             (action sock)

-- |withUDPSocket opens a new UDP socket, with the promise that it will be
-- closed in the event of an exception during the execution of the passed
-- action. One the socket leaves the execution of the action, it is no longer
-- guaranteed to be closed.
withProtectedUDPSocket :: (Socket -> IO a) -> IO a
withProtectedUDPSocket = bracket
  (socket AF_INET Datagram defaultProtocol)
  close

-- |withBoundUDPSocket opens a new UDP socket and binds it to an address
withProtectedBoundUDPSocket :: (Socket -> IO a) -> IO a
withProtectedBoundUDPSocket action = withProtectedUDPSocket $ \sock ->
  bracket_ (bind sock $ SockAddrInet aNY_PORT iNADDR_ANY)
                 (close sock)
                 (action sock)

describeSocket :: Socket -> IO (Addr, PortNumber)
describeSocket sock = do
  (SockAddrInet p ha) <- getSocketName sock
  let (a, b, c, d) = hostAddressToTuple ha
  return $ (Addr a b c d, p)

decompose :: SockAddr -> Maybe (Addr, PortNumber)
decompose sa = case sa of
  SockAddrInet p h -> let (a,b,c,d) = hostAddressToTuple h in
    return (addrW8 a b c d, p)
  _ -> Nothing

resolveAddress :: String -> Maybe PortNumber -> IO (Maybe SockAddr)
resolveAddress host service = do
  results <- getAddrInfo Nothing (Just host) (fmap show service)
  case results of
    (x:_) -> case (addrAddress x) of
      sa@(SockAddrInet _ _) -> return $ Just sa
      _ -> return Nothing
    _ -> return Nothing
