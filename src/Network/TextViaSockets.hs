{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.TextViaSockets
-- Copyright   :  (c) Damian Nadales 2017
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Damian Nadales <damian.nadales@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- Simple line-based text communication via sockets.
-----------------------------------------------------------------------------

module Network.TextViaSockets
    ( Connection ()
    -- * Connect to a server
    , connectTo
    , connectToWithRetry
    -- * Start a server
    , acceptOn
    , acceptOnSocket
    , getFreeSocket
    -- * Sending and receiving data
    , getLineFrom
    , putLineTo
    -- * Closing the connection
    , close   
    ) where

import Network.Socket hiding (recv, close, send)
import qualified Network.Socket as Socket
import Network.Socket.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Data.Maybe
import Data.Text.Encoding.Error
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Exception.Base
import System.IO.Error

-- | A connection for sending and receiving @Text@ lines.    
data Connection = Connection
    { connSock :: Socket
    , linesTQ :: TQueue Text
    , socketReaderTid :: ThreadId
    } deriving (Eq)

instance Show Connection where
    show Connection {connSock} =
        "Connection: socket = " ++ show connSock

-- | Accept byte-streams by serving on the given port number. This function
-- will block until a client connects to the server.
-- 
acceptOn :: PortNumber -> IO Connection
acceptOn p = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet p iNADDR_ANY)
    acceptOnSocket sock

-- | Like @acceptOn@ but it takes a bound socket as parameter.
acceptOnSocket :: Socket -> IO Connection
acceptOnSocket sock = do
    listen sock 1 -- Only one queued connection.
    (conn, _) <- accept sock 
    mkConnection conn

-- | Get a free socket from the operating system.
getFreeSocket :: IO Socket
getFreeSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
    return sock

-- | Connect to the given host and service name (usually a port number). 
--
connectTo :: HostName -> ServiceName -> IO Connection
connectTo host sn = withSocketsDo $ do
    -- Open the socket.
    addrinfos <- getAddrInfo Nothing (Just host) (Just sn)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    mkConnection sock

-- | Like connect to, but if the server is not available it retries a number of
-- times before raising an exception.
connectToWithRetry :: HostName -> ServiceName -> IO Connection
connectToWithRetry host sn = gConnectToWithRetry (3 :: Int)
    where gConnectToWithRetry 0 = fail $ "Could not connect to "
                                       ++ show host ++ ":" ++ show sn
          gConnectToWithRetry n = connectTo host sn `catch` handler
              where 
                handler :: IOException -> IO Connection
                handler _ = do
                    threadDelay (10^6)
                    gConnectToWithRetry (n-1)

mkConnection :: Socket -> IO Connection
mkConnection sock = do
    -- Create an empty queue of lines.
    lTQ <- newTQueueIO    
    -- Spawn the reader process.
    rTid <- forkIO $ reader sock lTQ [] (streamDecodeUtf8With lenientDecode) 
    return $ Connection sock lTQ rTid
    where
      -- | Reads byte-strings from the given socket, decodes the byte-string
      -- into a @Text@ value, and as soon as a new line is found in the text,
      -- the line is placed in the given @TQueue@.
      reader :: Socket      -- ^ Socket on which the byte-strings will be received.
             -> TQueue Text -- ^ Transactional queue where to put the text
                            -- lines that are received.
             -> [Text]      -- ^ Text fragments that were received so far,
                            -- where no new lines are found
             -> (ByteString -> Decoding) -- ^ Decoding function. See @Data.Text.Encoding@
             -> IO ()
      reader sock lTQ acc f = doRead acc f `catch` handler
          where doRead acc' f' = do
                    msg <- recv sock 1024
                    -- Receiving a null byte-string probably means that the
                    -- sending side has closed the connection.
                    unless (B.null msg) $ do
                        let Some text _ g = f' msg
                        rest <- putLines lTQ text acc'
                        doRead rest g
                handler :: IOException -> IO ()
                handler _ = return () -- TODO: for now we're not notifying that the reader has died.

      -- | If a new-line is found in @text@, put @text@ together with the
      -- remainder date in @acc@ as one line in @lTQ@. The buffer @acc@ stores
      -- the line fragments as a stack, so it is necessary to reverse this list
      -- before concatenating all the fragments together.
      putLines lTQ text acc =
          if isNothing $ T.find (=='\n') text
              then return (text:acc) -- The text does not contain a new line,
                                     -- so we add it to the front to the
                                     -- fragments list. This means that the
                                     -- text fragments will appear in the
                                     -- reverse order, so it is necessary to
                                     -- reverse the elements when forming the
                                     -- whole line with these fragments.
              else do
              let (suffix, remainder) = T.break (== '\n') text
                  line = T.concat (reverse (suffix:acc)) -- Note that we're
                                                         -- reversing the
                                                         -- buffer here.
              atomically $ writeTQueue lTQ line
              putLines lTQ (T.tail remainder) [] -- We take the tail of the
                                                 -- remaining fragment to
                                                 -- discard the new line
                                                 -- character.

-- | Read a text line from the given connection.
--
-- This function might throw an @BlockedIndefinitelyOnSTM@ exception if the
-- connection to the server is closed, so users of this function should check
-- for this.
getLineFrom :: Connection -> IO Text
getLineFrom Connection {linesTQ} = atomically $ readTQueue linesTQ

-- | Put a text line onto the given connection.
putLineTo :: Connection -> Text -> IO ()
putLineTo Connection {connSock} text = do
    let textEol = T.snoc text '\n'
    sendAll connSock (encodeUtf8 textEol)

-- | Close the connection.
close :: Connection -> IO ()
close Connection{connSock, socketReaderTid} = do
    Socket.close connSock
    killThread socketReaderTid
