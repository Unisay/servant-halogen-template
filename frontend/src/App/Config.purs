module App.Config where

import Preamble

import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import FusionAuth (ApiKey, ApiUrl, ApplicationId, User)

type Config = 
  { logLevel :: LogLevel 
  , applicationId :: ApplicationId
  , apiUrl :: ApiUrl
  , fusionAuthApiUrl :: ApiUrl
  , fusionAuthApiKey :: ApiKey
  , userEnv :: UserEnv
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

type UserEnv =
  { currentUser :: Ref (Maybe User)
  , userBus :: BusRW (Maybe User)
  }