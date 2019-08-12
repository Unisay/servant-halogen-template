module Server.Api
  ( Api
  ) where

import Domain
import Servant.API

import Servant.Server.Experimental.Auth (AuthServerData)
import Server.Auth                      (JwtPayload)


type ProtectedApi api = AuthProtect "jwt-auth" :> api

type UnprotectedExecApi = "execute"
  :> Capture "target" ExecTarget
  :> ReqBody '[JSON] ExecConfig
  :> Post '[JSON] ExecResult

type instance AuthServerData (AuthProtect "jwt-auth") = JwtPayload

type Api = ProtectedApi UnprotectedExecApi
