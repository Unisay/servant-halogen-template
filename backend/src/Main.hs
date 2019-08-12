module Main
  ( run
  )
where

import Preamble

import Control.Exception                (bracket)
import Network.Wai                      (Application, Request)
import Servant                          (Context (..), serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler)
import Server                           (server)
import Server.Api                       (Api)
import Server.Auth                      (JwtPayload, authHandler)
import System                           (EnvVariables (..), loadEnvVariables)
import Types                            (Config (..), unTcpPort)

import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Remote.Monitoring as EKG


run :: IO ()
run = bracket initConfiguration shutdownConfiguration $ \config -> do
  waiApplication <- mkWaiApplicatoin config
  let port = unTcpPort $ _configApiServerPort config
  putText $ "Starting server at http://localhost:" <> show port
  Warp.run port waiApplication

mkWaiApplicatoin :: Config -> IO Application
mkWaiApplicatoin config =
  return $ serveWithContext (Proxy @Api) (genAuthServerContext config) server

genAuthServerContext :: Config -> Context (AuthHandler Request JwtPayload ': '[])
genAuthServerContext config = authHandler config :. EmptyContext

initConfiguration :: IO Config
initConfiguration = do
  envVars <- loadEnvVariables
  let ekgServerPort = unTcpPort $ _envVariablesEkgServerPort envVars
  ekgServer <- EKG.forkServer "localhost" ekgServerPort
  pure Config { _configApiServerPort = _envVariablesApiServerPort envVars
              , _configEnvironment   = _envVariablesEnvironment envVars
              , _configEkgServer     = EKG.serverThreadId ekgServer
              , _configAuthApi       = _envVariablesAuthApi envVars
              }

shutdownConfiguration :: Config -> IO ()
shutdownConfiguration config = do
  let ekgThreadId = _configEkgServer config
  putText $ "\nStopping EKG process by " <> show ekgThreadId
  killThread ekgThreadId
  putText "EKG process stopped"
