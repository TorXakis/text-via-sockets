{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Concurrent.Async
import           Network.Socket hiding (close)
import           Control.Monad
import           Control.Exception.Base
import           System.Timeout
import qualified Network
import           Data.Foldable
import           System.IO    

import           Criterion.Main

import           Network.TextViaSockets


sendStrLines :: [String] -> IO ()
sendStrLines strs = withSocketsDo $ benchTimeout $ do
    sock <- Network.listenOn (Network.PortNumber 8888)
    aSvr <- async $ Network.accept sock
    cliH <- Network.connectTo "localhost" (Network.PortNumber 8888)
    (svrH, _, _) <- wait $ aSvr
    -- Send all the lines
    aSnd <- async $ traverse_ (hPutStrLn svrH) strs
    -- Receive all the lines
    msgs <- replicateM (length strs) (hGetLine cliH)
    -- Check the messages
    when (length strs /= length msgs) $
        error "The number of sent and received strings don't match."
    -- Close the connections   
    _ <- wait aSnd
    hClose svrH
    hClose cliH
    Network.sClose sock


benchTimeout :: IO () -> IO ()
benchTimeout = void . timeout (2 * 10 ^ (6 :: Int))    
    
sendTLines :: [Text] -> IO ()
sendTLines tLines = void $ timeout (2 * 10 ^ (6 :: Int)) $ do
    sock <- getFreeSocket
    acceptAsync <- async $ acceptOnSocket sock
    pnum <-  socketPort sock
    cliConn <- connectTo "localhost" (show pnum)
    svrConn <- wait acceptAsync
    -- Server: send all the messages to the client.
    sendAssync <- async $ sendMsgs svrConn tLines
    -- Client: receive all the messages from the server.
    msgs <- receiveMsgs cliConn (length tLines)
    when (length tLines /= length msgs) $
        error "The number of sent and received messages don't match."
    -- Close the connections
    wait sendAssync
    close cliConn `catch` handler
    close svrConn `catch` handler
    where
      -- | For benchmarking purposes we just swallow the exceptions (most likely `epollControl`).
      handler :: IOException -> IO ()
      handler ex = putStrLn $ "Benchmarks: exception while closing the connections: " ++ show ex
          

main :: IO ()
main =
    defaultMain [ bench "Text: Lorem Ipsum." $
                    nfIO (sendTLines loremIpsum)
                , bench "Text: Lorem Ipsum x100." $
                    nfIO (sendTLines (loremIpsumN 100))
                , bench "String: Lorem Ipsum." $
                  nfIO (sendStrLines loremIpsumStr)
                , bench "String: Lorem Ipsum x100." $
                  nfIO (sendStrLines (loremIpsumStrN 100))                  
                ]

loremIpsumStrN :: Int -> [String]
loremIpsumStrN n = concat (replicate n loremIpsumStr)

loremIpsumStr :: [String]
loremIpsumStr = map T.unpack loremIpsum

loremIpsumN :: Int -> [Text]
loremIpsumN n  = concat (replicate n loremIpsum)

-- | Text to use in the benchmarks.
loremIpsum :: [Text]
loremIpsum =
    [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed ullamcorper dolor et bibendum egestas. Nunc condimentum luctus erat vitae fermentum. Quisque malesuada laoreet neque et egestas. Duis lacinia dignissim scelerisque. Aliquam id fringilla lacus, vitae ultrices neque. Aliquam condimentum vulputate quam, nec varius eros condimentum in. In vel finibus dolor. Nulla vulputate orci quis bibendum elementum. Cras convallis ornare nisl sed malesuada. Etiam vulputate odio rutrum, mattis quam in, mattis dui. Nulla quis erat risus. Vivamus non tortor odio. Suspendisse laoreet arcu nec interdum pharetra. Vivamus tristique lacinia faucibus. Quisque imperdiet malesuada mi, non varius sapien tincidunt vitae. Quisque placerat, est eu vestibulum mollis, ante metus imperdiet velit, eu gravida magna magna eu lectus."
    , "Ut elementum pretium sapien, vel ullamcorper turpis. Ut ac erat nec nisi luctus congue ac in nisl. Curabitur sapien nisi, fermentum sed bibendum nec, placerat ac leo. Maecenas pretium dui non blandit consectetur. Suspendisse tempor diam justo, et lobortis lectus ultricies sed. Vestibulum sit amet dui ut ex euismod dapibus. Donec accumsan dui eu convallis ullamcorper. Nullam scelerisque volutpat ligula in varius. Nulla vestibulum libero ut libero tincidunt, vel tincidunt enim laoreet. Fusce quis tellus quis risus vulputate mollis at at quam. Vestibulum leo risus, mattis sit amet ligula vel, luctus fringilla quam. Suspendisse luctus eu nisi ac tincidunt. Nunc elementum lectus tellus, ac pretium eros elementum ac. Vestibulum elementum neque id cursus imperdiet."
    , "Duis non ullamcorper mi, non malesuada lectus. Donec ac nisi eu felis faucibus tristique. Suspendisse potenti. Phasellus ultrices blandit luctus. Mauris dapibus maximus aliquam. Aliquam scelerisque purus ac mollis suscipit. Cras pharetra maximus eleifend. Curabitur suscipit gravida ligula, sed tincidunt lectus malesuada in. Suspendisse vitae scelerisque nisi."
    , "Duis efficitur elit metus, at mattis lacus ultricies sit amet. Mauris egestas iaculis mi at viverra. Curabitur ipsum lorem, auctor nec est id, accumsan elementum velit. Ut diam lacus, pharetra non ante non, tincidunt pulvinar augue. Donec vulputate lobortis suscipit. Curabitur ut varius purus. Nunc ullamcorper nisi sed orci egestas cursus. Mauris iaculis risus purus, nec fringilla velit consequat commodo."
    , "Maecenas egestas id velit laoreet accumsan. Pellentesque eget mollis urna. Duis vel ex dui. Mauris nec ex non nisl sagittis luctus eget vel eros. Sed quis mauris magna. Sed aliquet in risus id accumsan. Nam non odio nunc. Nullam mollis pulvinar nulla, tempus ornare eros molestie in. "
    ]
