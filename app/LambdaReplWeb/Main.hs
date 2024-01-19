{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main ( main ) where

import LambdaFrontend.REPL ( ReplState, defaultReplState, processString )

import System.IO ( hFlush, stdout )

import qualified Network.WebSockets as WS
import Network.WebSockets.Snap ( runWebSocketsSnap )

import Snap.Http.Server ( httpServe )
import Snap.Http.Server.Config


import Control.Exception.Base ( finally )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Options.Applicative


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



data ProgramOptions = ProgramOptions {
    host_ :: String,
    port_ :: Int,
    sslPort_ :: Maybe Int,
    sslCert_ :: Maybe String,
    sslKey_ :: Maybe String
  }

programOptionsParser :: Parser ProgramOptions
programOptionsParser = ProgramOptions
  <$> strOption ( long "host" <> short 'h' )
  <*> option auto ( long "port" <> short 'p' )
  <*> optional ( option auto $ long "ssl-port" )
  <*> optional ( strOption $ long "cert" )
  <*> optional ( strOption $ long "key" )


bsFromString :: String -> BS.ByteString
bsFromString = T.encodeUtf8 . T.pack

simpleConfig :: String -> Int -> Config m a
simpleConfig hostP nonSslPortP =
  let
    port = setPort nonSslPortP
    hostname = setHostname ( bsFromString hostP )
    accessLog = setAccessLog ( ConfigIoLog $ putStrLn . T.unpack . T.decodeUtf8 )
    errorLog = setErrorLog ( ConfigIoLog $ putStrLn . T.unpack . T.decodeUtf8 )
    locale = setLocale "US"
    verbose = setVerbose True

    l = [port, hostname, accessLog, errorLog, locale, verbose]
  in
    foldl (\a b -> b a) emptyConfig l

sslConfig :: String -> Int -> Int -> String -> String -> Config m a
sslConfig hostP nonSslPortP sslPortP certPathP keyPathP =
  let   
    sslPort = setSSLPort sslPortP
    ip = setSSLBind ( bsFromString "0.0.0.0" )
    cert = setSSLCert certPathP
    chainSert = setSSLChainCert True
    key = setSSLKey keyPathP

    l = [sslPort, ip, cert, chainSert, key]
  in
    foldl (\a b -> b a) ( simpleConfig hostP nonSslPortP ) l


entrypoint :: ProgramOptions -> IO()
entrypoint ( ProgramOptions host port ( Just sslPort ) ( Just certPath ) ( Just keyPath ) )
  = httpServe ( sslConfig host port sslPort certPath keyPath ) $ runWebSocketsSnap application
entrypoint ( ProgramOptions host port _ _ _ )
  = httpServe ( simpleConfig host port ) $ runWebSocketsSnap application


main :: IO()
main = execParser opts >>= entrypoint
  where
    opts = info (programOptionsParser <**> helper) fullDesc
