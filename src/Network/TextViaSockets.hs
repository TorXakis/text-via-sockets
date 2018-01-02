{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
import Data.Foldable
import Control.Retry
import Control.Monad.Catch (Handler)
import Data.Monoid

#ifdef DEBUG
import Debug.Trace
#else
import Debug.NoTrace
#endif

-- | A connection for sending and receiving @Text@ lines.
data Connection = Connection
    { -- | Socket on which to send and receive data.
      connSock :: ! Socket
      -- | Server socket. It exists only if the connection was started on server mode.
    , serverSocket :: !(Maybe Socket)
    , linesTQ :: !(TQueue Text)
    , socketReaderTid :: !ThreadId
    } deriving (Eq)

instance Show Connection where
    show Connection {connSock} =
        "Connection: socket = " ++ show connSock

-- | We always retry an IO Exception.
retryIOException :: IOException -> IO Bool
retryIOException _ = return True

-- | How to report an IOException.
reportIOException :: Bool        -- ^ Is the action being retried or are we giving up?
                  -> IOException -- ^ Exception that was raised.
                  -> RetryStatus -- ^ Contains the number of iterations, delay so far, and last delay.
                  -> IO ()
reportIOException _ ex rs = do
        traceIO $ "Got exception: " ++ show ex
        traceIO $ "Current delay: " ++ show (rsPreviousDelay rs)
        traceIO $ "Total delay: " ++ show (rsCumulativeDelay rs)

-- | The handler for IO exceptions when connecting to sockets.
ioExceptionHandler :: RetryStatus -> Handler IO Bool
ioExceptionHandler = logRetries retryIOException reportIOException

-- | Default retry policy for retrying connections.
connectRetryPolicy :: RetryPolicyM IO
connectRetryPolicy = exponentialBackoff 50 <> limitRetries 5

-- | Retry to connect
retryCnect :: IO a -> IO a
retryCnect act = recovering connectRetryPolicy [ioExceptionHandler] (\_ -> act)

-- | Accept byte-streams by serving on the given port number. This function
-- will block until a client connects to the server.
--
acceptOn :: PortNumber -> IO Connection
acceptOn p = retryCnect $ do
    traceIO $ ">>> Accepting a connection on " ++ show p
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet p iNADDR_ANY)
    acceptOnSocket sock

-- | Like @acceptOn@ but it takes a bound socket as parameter.
acceptOnSocket :: Socket -> IO Connection
acceptOnSocket sock = retryCnect $ do
    listen sock 1 -- Only one queued connection.
    (conn, _) <- accept sock
#ifdef DEBUG
    pn <- socketPort conn
    traceIO $ "<<< Accepted a connection on " ++ show pn
#endif
    mkConnection conn (Just sock)

-- | Get a free socket from the operating system.
getFreeSocket :: IO Socket
getFreeSocket = retryCnect $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
    return sock

-- | Connect to the given host and service name (usually a port number).
--
connectTo :: HostName -> ServiceName -> IO Connection
connectTo hn sn = withSocketsDo $ retryCnect $ do
    -- Open the socket.
    traceIO $ ">>> Connecting to " ++ show hn ++ " on " ++ show sn
    addrinfos <- getAddrInfo Nothing (Just hn) (Just sn)
    let svrAddr = head addrinfos
    sock <- socket (addrFamily svrAddr) Stream defaultProtocol
    connect sock (addrAddress svrAddr)
    traceIO $ "<<< Connected to " ++ show hn ++ " on " ++ show sn
    mkConnection sock Nothing

mkConnection :: Socket -> Maybe Socket -> IO Connection
mkConnection sock mServerSock = do
    -- Create an empty queue of lines.
    lTQ <- newTQueueIO
    -- Spawn the reader process.
    rTid <- forkIO $ reader lTQ [] (streamDecodeUtf8With lenientDecode)
    return $ Connection sock mServerSock lTQ rTid
    where
      -- | Reads byte-strings from the given socket, decodes the byte-string
      -- into a @Text@ value, and as soon as a new line is found in the text,
      -- the line is placed in the given @TQueue@.
      reader :: TQueue Text -- ^ Transactional queue where to put the text
                            -- lines that are received.
             -> [Text]      -- ^ Text fragments that were received so far,
                            -- where no new lines are found
             -> (ByteString -> Decoding) -- ^ Decoding function. See @Data.Text.Encoding@
             -> IO ()
      reader lTQ acc f = doRead acc f `catch` handler
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
#ifdef DEBUG
getLineFrom Connection {connSock, linesTQ} = do
#else
getLineFrom Connection {linesTQ} = do
#endif
    line <- atomically $ readTQueue linesTQ
#ifdef DEBUG
    pn <- socketPort connSock
    traceIO $ "Got line" ++ show line ++ " on " ++ show pn
#endif
    return line

-- | Put a text line onto the given connection.
putLineTo :: Connection -> Text -> IO ()
putLineTo Connection {connSock} text = do
    let textEol = T.snoc text '\n'
#ifdef DEBUG
    pn <- socketPort connSock
    traceIO $ ">>> Putting line " ++ show textEol ++ " on " ++ show pn
#endif
    sendAll connSock (encodeUtf8 textEol)

-- | Close the connection.
close :: Connection -> IO ()
close Connection{connSock, serverSocket, socketReaderTid} = retryCnect $ do
    Socket.close connSock
    traverse_ Socket.close serverSocket
    killThread socketReaderTid
