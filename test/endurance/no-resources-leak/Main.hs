-- | This test simply opens and closes connections on the same port for a
-- certain amount of time. The goal of this test is to make sure that no
-- resources are leaked.

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Exception.Base
import           Control.Concurrent
import           System.Remote.Monitoring
import           System.Environment
import           Network.Socket (socketPort)

import           Network.TextViaSockets

-- | Simple endurance test. To run it pass the @"simple"@ argument to it.
-- When using stack, this can be achieved by means of the @--ta@ flag:
--
-- > stack test text-via-sockets:no-resources-leak-test  --ta "simple"
--
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> reportSkipping
        "simple": _ -> simpleEnduranceTest
        "evil"  : _ -> evilTest
        _ -> reportSkipping
    where
      reportSkipping = putStrLn "Skipping endurance tests"
      -- Simple tests that opens and closes a connection on the same port.
      simpleEnduranceTest = do
          _ <- forkServer "localhost" 8000
          forever $ handle delay $ do
              putStrLn "Opening connections..."
              a <- async $ acceptOn 6969
              cliConn <- connectTo "127.0.0.1" "6969"
              svrConn <- wait a
              putStrLn "Closing connections..."
              waitABit
              close cliConn
              waitABit
              close svrConn
              waitABit
          where
            delay :: IOException -> IO ()
            delay ex = do
                putStrLn $ "Got IO exception: " ++ show ex
                threadDelay $ 3 * 10 ^ (6 :: Int)
                putStrLn "Retrying ..."          
                return ()
      waitABit = threadDelay $ 5 * 10 ^ (5 :: Int)
      -- Test that will leak resources on purpose, mainly use to test that
      -- resource leaks are actually reported.
      evilTest = do
          _ <- forkServer "localhost" 8000
          forever $ do
              waitABit
              sock <- getFreeSocket
              a <- async $ acceptOnSocket sock
              pnum <-  socketPort sock
              _ <- connectTo "localhost" (show pnum)
              _ <- wait a
              return ()
          
