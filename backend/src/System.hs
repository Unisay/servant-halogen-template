module System
  ( EnvVariables(..)
  , loadEnvVariables
  ) where


import Prelude
import System.Envy
import Types

data EnvVariables = EnvVariables
  { _envVariablesApiServerPort :: TcpPort
  , _envVariablesEkgServerPort :: TcpPort
  , _envVariablesEnvironment   :: Environment
  }

instance FromEnv EnvVariables where
  fromEnv = EnvVariables
    <$> env "API_PORT"
    <*> env "EKG_PORT"
    <*> env "ENV"

loadEnvVariables :: IO EnvVariables
loadEnvVariables = do
  vars :: Either String EnvVariables <- decodeEnv
  either error pure vars


