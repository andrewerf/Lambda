module Main ( main ) where

import LambdaFrontend.REPL ( ReplState, defaultReplState, processString )

import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )

import qualified Network.WebSockets as WS

import Control.Exception.Base ( finally )
import Data.Text as T


talk :: WS.Connection -> IO ()
talk = go defaultReplState
  where
    go :: ReplState -> WS.Connection -> IO()
    go st conn = do
      msg <- T.unpack <$> WS.receiveData conn
      ( newSt, resp ) <- processString st msg
      putStrLn $ "Client entered: " ++ msg ++ "\n\twe responded: " ++ resp
      hFlush stdout
      WS.sendTextData conn $ T.pack resp
      go newSt conn


application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  putStrLn "Client connected"
  hFlush stdout
  WS.withPingThread conn 30 ( return () ) $ do
    finally ( talk conn ) disconnect
  where
    disconnect = do
      putStrLn "Client disconnected"
      hFlush stdout


main :: IO()
main = do
  [host, port] <- getArgs
  WS.runServer host ( read port ) application
