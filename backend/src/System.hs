module System
  ( EnvVariables(..)
  , loadEnvVariables
  ) where


import Preamble
import System.Envy
import Types

import Data.String   (String)
import Prelude       (error)


data EnvVariables = EnvVariables
  { _envVariablesApiServerPort :: TcpPort
  , _envVariablesEkgServerPort :: TcpPort
  , _envVariablesEnvironment   :: Environment
  , _envVariablesAuthApi       :: BaseUrl
  }

instance FromEnv EnvVariables where
  fromEnv _ =
    EnvVariables <$> env "API_PORT" <*> env "EKG_PORT" <*> env "ENV" <*> env
      "AUTH_API"

loadEnvVariables :: IO EnvVariables
loadEnvVariables = do
  vars :: Either String EnvVariables <- decodeEnv
  either error pure vars


