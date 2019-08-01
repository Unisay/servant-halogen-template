module Server.Api
  ( UserApi
  ) where

import Domain      (User, UserData)
import Servant.API ((:>), JSON, Post, ReqBody)

type ProtectedApi api = api

type UnprotectedUserApi = "users"
  :> ReqBody '[JSON] UserData
  :> Post '[JSON] User

type UserApi = ProtectedApi UnprotectedUserApi
