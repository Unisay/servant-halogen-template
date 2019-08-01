module Main (run) where

import           Control.Exception        (bracket)
import           Network.Wai              (Application)
import           Preamble
import           Servant                  (serve)
import           Server                   (server)
import           Server.Api               (UserApi)
import           System                   (EnvVariables (..), loadEnvVariables)
import           Types

import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Remote.Monitoring as EKG


run :: IO ()
run = bracket initConfiguration shutdownConfiguration $ \config -> do
  waiApplication <- mkWaiApplicatoin config
  let port = unTcpPort $ _configApiServerPort config
  putText $ "Starting server at http://localhost:" <> show port
  Warp.run port waiApplication

mkWaiApplicatoin :: Config -> IO Application
mkWaiApplicatoin _ = return $ serve (Proxy @UserApi) server

-- | Allocates resources for 'Env'
initConfiguration :: IO Config
initConfiguration = do
  envVars <- loadEnvVariables
  let ekgServerPort = unTcpPort $ _envVariablesEkgServerPort envVars
  ekgServer <- EKG.forkServer "localhost" ekgServerPort
  pure Config
    { _configApiServerPort = _envVariablesApiServerPort envVars
    , _configEnvironment = _envVariablesEnvironment envVars
    , _configEkgServer = EKG.serverThreadId ekgServer
    }

shutdownConfiguration :: Config -> IO ()
shutdownConfiguration config = do
  let ekgThreadId = _configEkgServer config
  putText $ "\nStopping EKG process by " <> show ekgThreadId
  killThread ekgThreadId
  putText "EKG process stopped"
