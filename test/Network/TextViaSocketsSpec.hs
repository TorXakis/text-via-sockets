{-# LANGUAGE CPP #-}
module Network.TextViaSocketsSpec (spec) where

import           Control.Concurrent
import           Data.Text (Text)
import           Network.Socket hiding (close)
import           Control.Exception.Base hiding (assert)
import           Control.Concurrent.Async
import           qualified Data.Text as T

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Data.Text.Arbitrary ()

import           Network.TextViaSockets

#ifdef DEBUG
import Debug.Trace
#else
import Debug.NoTrace
#endif

-- | Timeout token.
data Timeout = Timeout

-- | A process that sends and receives data to and from another process, @p@.
sndRcvProc :: Connection     -- ^ Connection to @p@.
           -> Int            -- ^ Number of messages to be received from @p@.
           -> [Text]         -- ^ Messages to send to @p@.
           -> MVar (Async a) -- ^ Async handle to @p@. If an exception arises
                             -- at this process, @p@ has to be canceled.
           -> IO (Either Timeout [Text])
sndRcvProc conn howMany svrMsgs aTV =
    timeout `race` (sndRcvProc' `catch` handler)
    where
      sndRcvProc' = do
          a <- async $ sendMsgs conn svrMsgs
          res <- receiveMsgs conn howMany
          wait a
          return res
    -- In case of an IOException we have to cancel the process that is waiting
    -- for input.
      handler :: IOException -> IO [Text]
      handler ex = do
          a <- takeMVar aTV
          cancel a
          throwIO ex

checkMessages :: Either a (Either Timeout [Text]) -> [Text] -> PropertyM IO ()
checkMessages (Left _) _ = monitor $ collect ("Address in use." :: String)
checkMessages (Right (Left Timeout)) _ = monitor $ collect ("Timeout." :: String)
checkMessages (Right (Right msgs)) expected =
    if msgs == expected
    then do
        monitor $ collect ("Successful connection." :: String)
        assert True
    else do
        run $ print msgs
        run $ print expected
        assert False

allMessagesReceived :: [PrintableString] -- ^ Messages to be sent to the client.
                    -> [PrintableString] -- ^ Messages to be sent to the server.
                    -> Property
allMessagesReceived strsCli strsSvr = monadicIO $ do
    debug "Acquiring connections."
    (cliConn, svrConn, aCliTV, aSvrTV) <- run getCliSvrConns
    -- Start the sending and receiving processes.
    debug "Starting the sending and receiving processes."
    aCli <- run $ async $ sndRcvProc cliConn (length msgsCli) msgsSvr aSvrTV
    aSvr <- run $ async $ sndRcvProc svrConn (length msgsSvr) msgsCli aCliTV
    -- Put the async handles in the MVar's
    debug "Putting the async handles in the MVar's."
    run $ putMVar aCliTV aCli
    run $ putMVar aSvrTV aSvr
    -- Wait for the results
    debug "Waiting for the results."
    resCli <- run $ waitCatch aCli
    resSvr <- run $ waitCatch aSvr
    -- Close the connections
    debug "Closing the connections."
    run $ close cliConn
    run $ close svrConn
    -- Check the results
    debug "Checking the results."
    checkMessages resCli msgsCli
    checkMessages resSvr msgsSvr
    where
      strsCliF = filter ((/= "\n") . getPrintableString) strsCli
      strsSvrF = filter ((/= "\n") . getPrintableString) strsSvr
      msgsCli = map (T.pack . getPrintableString) strsCliF
      msgsSvr = map (T.pack . getPrintableString) strsSvrF
      debug = run . traceIO . ("TextViaSocketsSpec: allMessagesReceived: "++)

getCliSvrConns :: IO (Connection, Connection, MVar (Async a), MVar (Async a))
getCliSvrConns = do
    aCliTV <- newEmptyMVar
    aSvrTV <- newEmptyMVar
    sock <- getFreeSocket
    a <-  async $ acceptOnSocket sock
    pnum <-  socketPort sock
    cliConn <- connectTo "localhost" (show pnum)
    svrConn <- wait a
    return (cliConn, svrConn, aCliTV, aSvrTV)

clientReceivesAll :: [PrintableString] -> Property
clientReceivesAll strs =
    allMessagesReceived strs []

timeout :: IO Timeout
timeout = do
    threadDelay t
    return Timeout
    where t :: Int
          t = 3 * 10 ^ (6 :: Int)

serverReveivesAll :: [PrintableString] -> Property
serverReveivesAll =
    allMessagesReceived []

spec :: Spec
spec = do
    describe "Good weather messages reception:" $ do
        it "The client receives all the messages" $
            property clientReceivesAll
        it "The server receives all the messages" $
            property serverReveivesAll
        it "The server and client receive all the messages" $
            property allMessagesReceived
    describe "Closing:" $ do
        it "The same port can be used after a connection is closed" $ do
            let simpleTest = do
                    a <- async $ do
                        svrConn <- acceptOn 9090
                        line <- getLineFrom svrConn
                        close svrConn
                        return line
                    cliConn <- connectTo "127.0.0.1" "9090"
                    putLineTo cliConn "Hello"
                    line <- wait a
                    close cliConn
                    line `shouldBe` "Hello"
            simpleTest -- Use the port the first time
            simpleTest -- And use it a second time
        it "Closing a connection two times should not fail" $ do
            (cliConn, svrConn, _, _) <- getCliSvrConns
            close cliConn
            close svrConn
            close svrConn
            close cliConn
